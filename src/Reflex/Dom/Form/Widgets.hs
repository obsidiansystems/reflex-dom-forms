{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Reflex.Dom.Form.Widgets where

import           Control.Monad.Fix          (MonadFix)
import           Data.Bifunctor             (first)
import           Data.Maybe                 (isJust)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified GHCJS.DOM.Types            as DOM
import           Reflex.Dom.Core
 ( DomBuilder, DomBuilderSpace, Dynamic, Event, GhcjsDomSpace, MonadHold
 , Performable, PerformEvent, PostBuild, Reflex, blank, dynText, elAttr
 , elDynAttr, ffor, fmapMaybe, holdDyn, holdUniqDyn, maybeDyn, updated
 , widgetHold, (=:)
 )
import           Reflex.Dom.Form.Validators (Validator (..))
import qualified Reflex.Dom.TextField       as TxtField

-- | Like 'formItem'' but assumes empty @class@ attribute.
formItem
  :: ( DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m
     , DOM.MonadJSM (Performable m)
     , PerformEvent t m, MonadFix m, MonadHold t m, Eq a
     )
  => m (Dynamic t (Either Text a), Event t (Maybe Text))
  -> m (Dynamic t (Either Text a))
formItem = formItem' ""

-- | Creates a form item that shows errors for invalid values.
formItem'
  :: ( DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m
     , DOM.MonadJSM (Performable m)
     , PerformEvent t m, MonadFix m, MonadHold t m, Eq a
     )
  => Text
  -- ^ @class@ attribute for form item
  -> m (Dynamic t (Either Text a), Event t (Maybe Text))
  -- ^ A form widget producing a 'Dynamic' that can be 'Either' valid
  -- ('Right') or invalid ('Left'), and its underlying element.
  -> m (Dynamic t (Either Text a))
formItem' classes_ input = mdo
  (val, eInvalidMMsg) <- elDynAttr "div" fieldClasses $ do
    valEl_ <- input

    widgetHold blank $ ffor (updated latestErr) $ \case
      Nothing -> pure ()
      Just dErrMsg -> elAttr "div" ("class"=:"ui basic red pointing prompt label visible transition") (dynText dErrMsg)

    pure valEl_

  -- Keep the latest error message to prevent updating the DOM
  latestErr <- maybeDyn =<< holdUniqDyn =<< holdDyn Nothing eInvalidMMsg

  -- Initially hide error messages since the input is unchanged; show the message once typing starts.
  let mkFieldClasses showErr = "class"=:("field" <> (if showErr then " error " else " ") <> classes_)
      fieldClasses = mkFieldClasses . isJust <$> latestErr

  pure val


-- | Builds an @input@ element with validated result.
validatedInput
  :: ( DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m
     , DOM.MonadJSM (Performable m), PerformEvent t m, MonadFix m
     )
  => Validator t m a -- A 'Validator' to use for validating and configuring the @input@
  -> TxtField.TextField t m -- A base 'TextField' to use as the @input@ configuration.
  -> m (Dynamic t (Either Text a))
validatedInput (Validator check mods) cfg = mdo
  let cfg' = TxtField.setInvalidEvent err cfg
  v <- (fmap . fmap) check $ TxtField.mkField (mods cfg')
  let err = ffor (updated v) $ \case
        Left e -> e
        Right _ -> ""
  pure v

validatedInputCustomErrorDisplay
  :: ( DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m
     , DOM.MonadJSM (Performable m), PerformEvent t m, MonadFix m
     )
  => Validator t m a -- A 'Validator' to use for validating and configuring the @input@
  -> TxtField.TextField t m -- A base 'TextField' to use as the @input@ configuration.
  -> m (Dynamic t (Either Text a), Event t (Maybe Text))
validatedInputCustomErrorDisplay (Validator check mods) cfg = mdo
  let cfg' = TxtField.setInvalidEvent err cfg
  vAndE@(v,_) <- (fmap . first . fmap) check $
    TxtField.mkFieldCustomErrorDisplay (mods cfg')
  let err = ffor (updated v) $ \case
        Left e -> e
        Right _ -> ""
  pure vAndE


latestLeft :: (Reflex t, MonadHold t m) => a -> Event t (Either a b) -> m (Dynamic t a)
latestLeft initial x = holdDyn initial $ fmapMaybe (either Just (const Nothing)) x
