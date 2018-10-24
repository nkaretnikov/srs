{-# language TemplateHaskell #-}

module Main where

import Brick
import Control.Lens
import Graphics.Vty hiding (Event)
import System.Environment
import System.Random.Shuffle

percent :: Int -> Int -> Int
percent x tot = x * 100 `div` tot

strip :: String -> String
strip s = reverse $ f $ reverse $ f s
  where
    f = dropWhile (== ' ')

-- Not used.
type Event = ()
type Name = ()

data Card = Card
  { _question :: String
  , _answer   :: String
  }
makeLenses ''Card

data Typ
  = Question -- ^ question showed, wait for <Enter> to reveal answer
  | Answer   -- ^ answer showed, wait for (y/n) to determine if correct

-- | Application state.
data State = State
  { _typ     :: Typ     -- ^ question or answer?
  , _correct :: Int     -- ^ number of correct answers
  , _ui      :: String  -- ^ what to show in the UI
  , _cards   :: [Card]  -- ^ list of questions and answers
  }
makeLenses ''State

app :: App State Event Name
app = App
  { appDraw         = uiDraw
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = uiAttrMap
  }

mkQuestion :: [Card] -> String
mkQuestion cs =
  concatMap (++ "\n")
    [ (head cs) ^. question
    , "<Enter> to reveal answer"
    ]

-- XXX: Remove the 'ev' prefix when in a separate module.
evQuestion :: State -> EventM Name (Next State)
evQuestion s =
  continue $ s & ui  .~ (mkQuestion $ s ^. cards)
               & typ .~ Question

mkAnswer :: [Card] -> String
mkAnswer cs =
  concatMap (++ "\n")
    [ (head cs) ^. answer
    , "Correct? (y/n)"
    ]

evAnswer :: State -> EventM Name (Next State)
evAnswer s =
  continue $ s & ui    %~ (++ (mkAnswer $ s ^. cards))
               & cards %~ tail
               & typ   .~ Answer

incCorrect :: State -> State
incCorrect s = s & correct +~ 1

-- | Increment the counter on correct answers, otherwise just continue.
handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@(State _        _ _ []) _
  = halt s
handleEvent s@(State Question _ _ _)  (VtyEvent (EvKey KEnter []))
  = evAnswer s
handleEvent s@(State Answer   _ _ _)  (VtyEvent (EvKey (KChar 'y') []))
  = return (incCorrect s) >>= evQuestion
handleEvent s@(State Answer   _ _ _)  (VtyEvent (EvKey (KChar 'n') []))
  = evQuestion s
handleEvent s                         (VtyEvent (EvKey (KChar 'q') []))
  = halt s      -- exit
handleEvent s                         (VtyEvent (EvKey KEsc []))
  = halt s      -- exit
handleEvent s                         _
  = continue s  -- ignore

-- XXX: Show card number, total.
-- XXX: Show controls: q/esc.
uiDraw :: State -> [Widget Name]
uiDraw s = [str $ s ^. ui]

uiAttrMap :: a -> AttrMap
uiAttrMap = const $ attrMap defAttr []

initState :: [Card] -> State
initState [] = error "No cards to show"
initState cs = State
  { _typ     = Question
  , _correct = 0
  , _ui      = mkQuestion cs
  , _cards   = cs
  }

mkCard :: String -> Card
mkCard s = Card
  { _question = if not (null q') then q' else error "No question"
  , _answer   = if not (null a') then a' else error "No answer"
  }
  where
    (q, _:a) = break (== ';') s  -- XXX: hardcoded separator: ';'
    q' = strip q
    a' = strip a

main :: IO ()
main = do
  [file] <- getArgs
  cs <- readFile file >>= return . map mkCard . lines >>= shuffleM
  s <- customMain (mkVty defaultConfig) Nothing app (initState cs)
  let len = length cs
      cnt = s ^. correct
  putStrLn $ concat $
    [ show cnt, "/", show len
    , " (", show (percent cnt len), "%)"
    ]
