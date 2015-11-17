{-# LANGUAGE DataKinds, TypeOperators #-}

import Test.QuickCheck hiding ((===))
import Diagrams.Backend.SVG
import Diagrams hiding (Direction)
import Data.Colour.Names
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid.Base
import Data.Functor.Identity
import Data.Text (pack)
import qualified Data.Map as M

import System.IO.Unsafe
import Data.IORef

data Cell = C Integer Integer deriving (Ord, Eq, Show)
data Liveness = Dead | Living deriving (Eq, Show)

data Direction = N | E | S | W | NE | SE | NW | SW deriving (Enum, Show)

type Board = Cell -> Liveness

step :: Board -> Board
step board cell = case (mid, length (filter living neighbours)) of
                     (Living, l) | l == 2 || l == 3 -> Living
                     (Dead, 3) -> Living
                     _ -> Dead
  where neighbours = [board (go d cell) | d <- [N .. SW]]
        mid = board cell
        living :: Liveness -> Bool
        living Living = True
        living _ = False


memoed :: Board -> Board
memoed b = \c -> unsafePerformIO $
              do m <- readIORef mapRef
                 case M.lookup c m of
                   Nothing -> let new = b c in writeIORef mapRef (M.insert c new m) >> pure new
                   (Just old) -> pure old
   where mapRef :: IORef (M.Map Cell Liveness)
         mapRef = unsafePerformIO $ newIORef M.empty

go :: Direction -> Cell -> Cell
go N (C x y) = C x (y+1)
go E (C x y) = C (x+1) y
go W (C x y) = C (x-1) y
go S (C x y) = C x (y-1)
go SE (C x y) = C (x+1) (y-1)
go SW (C x y) = C (x-1) (y-1)
go NE (C x y) = C (x+1) (y+1)
go NW (C x y) = C (x-1) (y+1)

middle = (C 0 0)


testA@Dead = step a middle
  where a (C 0 0) = Living
        a (C 1 (-1)) = Living
        a _ = Dead

testB@Living = step b middle
  where b (C 0 0) = Living
        b (C 1 (-1)) = Living
        b (C (-1) 0) = Living
        b (C 0 (-1)) = Living
        b _ = Dead

testB'@Living = step b middle
  where b (C 0 0) = Living
        b (C (-1) 0) = Living
        b (C 0 (-1)) = Living
        b _ = Dead

testC@Dead = step c middle
  where c _ = Living

testD@Living = step d middle
  where d (C 1 (-1)) = Living
        d (C (-1) 0) = Living
        d (C 0 (-1)) = Living
        d _ = Dead

testD'@Dead = step d middle
  where d (C (-1) 0) = Living
        d (C 0 (-1)) = Living
        d _ = Dead

alltests = (testA, testB, testB', testC, testD, testD')


glider (C 0 1) = Living
glider (C 1 0) = Living
glider (C 1 (-1)) = Living
glider (C 0 (-1))  = Living
glider (C (-1) (-1))  = Living
glider _ = Dead


vis board (C x y) = cut 11 [case board (C x' y') of Living -> '@'; _ -> ' ' | y' <- reverse [y-5 .. y+5], x' <- [x-5 .. x+5]]

cut :: Int -> String -> [String]
cut n [] = []
cut n l = take n l : cut n (drop n l)

visM b c = mapM_ putStrLn (vis b c)

anim b = mapM_ (\b -> visM b middle >> putStrLn "-------------") (take 5 $ iterate step b)

prop_glider x y = (iterate (memoed . step) glider !! 4) (C (x+1) (y-1)) == glider (C x y)

c10 = circle 100.0 # fc green
w10 = circle 100.0

svgVis :: Board -> Cell -> Diagram SVG
svgVis board (C x y) = foldr1 (===) [line y' | y' <- reverse [y-window .. y+window]]
    where line y = foldr1 (|||) [translateX (fromIntegral dx) (case board (C (x+dx) y) of Living -> c10; _ -> w10) | dx <- [-window .. window]]
          window = 5

type LifeAPI = "glider" :> Capture "steps" Int
                        :> Capture "x" Integer
                        :> Capture "y" Integer :> Get '[HTML] DiagramSVG
           :<|> "chequered" :> Capture "steps" Int :> Get '[HTML] DiagramSVG
           :<|> "prime" :> Capture "prime" Integer :> Capture "steps" Int :> Get '[HTML] DiagramSVG
           :<|> "stripe" :> Capture "stripes" Integer :> Capture "steps" Int :> Get '[HTML] DiagramSVG
           :<|> "gcd" :> Capture "n" Integer :> Capture "steps" Int :> Get '[HTML] DiagramSVG


newtype DiagramSVG = Dia (Diagram SVG)

instance ToHtml DiagramSVG where
  toHtml = toHtmlRaw
  toHtmlRaw (Dia d) = relaxHtmlT (renderDia SVG opts d)
     where opts = (SVGOptions (mkWidth 250) Nothing (pack "") [] False)
           --generalise :: forall m . Applicative m => HtmlT Identity () -> HtmlT m ()
           --generalise = HtmlT . (pure :: a -> m a) . runIdentity . runHtmlT

main :: IO ()
main = do
  putStrLn "start serving"
  run 8080 (serve (Proxy :: Proxy LifeAPI) serveAPI)
  
 where serveAPI :: Server LifeAPI
       serveAPI = serveGlider :<|> serveChequered :<|> servePrime :<|> serveStripe :<|> serveGcd
       serveGlider steps x y = return (Dia (svgVis ((iterate (memoed . step) glider !! steps) ) (C x y)))
       serveChequered steps = return (Dia (svgVis ((iterate (memoed . step) chequered !! steps) ) middle))
       servePrime p steps = return (Dia (svgVis ((iterate (memoed . step) (prime p) !! steps) ) middle))
       serveStripe p steps = return (Dia (svgVis ((iterate (memoed . step) (prime p) !! steps) ) middle))
       serveGcd n = serveMiddle $ gcdB n
       serveMiddle board steps = return (Dia (svgVis ((iterate (memoed . step) board !! steps) ) middle))
       chequered (C x y) = case even $ x + y of True -> Living; _ -> Dead
       prime p (C x y) = case (x + y) `rem` p == 0 of True -> Living; _ -> Dead
       stripe p (C x y) = case (x + y) `div` p == 0 of True -> Living; _ -> Dead
       gcdB n (C x y) = case x `gcd` y == n of True -> Living; _ -> Dead
