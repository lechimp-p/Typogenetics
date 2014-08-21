module TyGen 
( Base (..)
, BString
, isPurin
, isPyrimidin
, complement
, isComplement
, Enzyme
, EnzymeCommand (..)
--, direction
, bindingBase
, bindsTo
--, Context
, initContext
--, apply
, execute
, stripBStrings 
)
where

import Data.List (splitAt)
import Data.List.Split (splitWhen)

-- Every genetic code consists of four bases

data Base = A | C | G | T
          deriving (Show, Read, Eq)

-- The bases are organized in BStrings

type BString = [Base]

-- There are two types of bases.

isPurin A = True
isPurin G = True
isPurin T = False 
isPurin C = False 

isPyrimidin A = False 
isPyrimidin G = False 
isPyrimidin T = True
isPyrimidin C = True

-- Every base has a complement

complement A = T
complement T = A
complement G = C
complement C = G

isComplement a = complement a == a

-- Enzyme act on BStrings and consist of commands

type Enzyme = [EnzymeCommand]

data EnzymeCommand = Cut -- Cut one or both BStrings after current position
                   | Del -- Delete base from BString at current position. 
                         -- Preceed with base right from current position.
                   | MoS -- Move enzyme to other BString
                   | MoR -- Move enzyme one base to the right
                   | MoL -- Move enzyme one to the left
                   | COn -- Start copy mode
                   | COf -- Stop copy mode
                   | InA -- Insert A right to current position
                   | InC -- Insert C right to current position
                   | InG -- Insert G right to current position
                   | InT -- Insert T right to current position
                   | PyR -- Search next pyrimidin to the right
                   | PyL -- Search next pyrimidin to the right
                   | PuR -- Search next purin to the right
                   | PuL -- Search next purin to the right
                   deriving (Show, Read)

-- Every Enzyme has a base it binds its start to, that is calculated based
-- on its structurce, i.e. its commands. 

-- Every commands steers the structure in a specific direction.
-- Directions are encoded as integers where 0 means right. There
-- are four directions at whole and they are counted in mathematical
-- clockwise direction.
direction Cut = 0 -- no change
direction Del = 0 
direction MoS = -1 -- turn right
direction MoR = 0
direction MoL = 0
direction COn = -1
direction COf = 1 -- turn left
direction InA = 0
direction InC = -1
direction InG = -1
direction InT = 1
direction PyR = -1
direction PyL = 1
direction PuR = 1
direction PuL = 1

bindingBase [] = A
bindingBase str = 
    let -- only use directions between commands
        directions = tail . fmap direction $ str
        finalDirection = specialSum directions `mod` 4
        specialSum [] = 0
        specialSum (_:[]) = 0
        specialSum (x:xs) = x + specialSum xs
    in case finalDirection of
        0 -> A
        1 -> C
        2 -> T
        3 -> G 

bindsTo a b = bindingBase a == b

-- We need a model for the execution of an enzyme on a bstring.
-- There are always two rows involved in the calculation, but
-- they could include more than one string:
-- GGATCTAGGTAG
--  ATG  TGCCT
-- So we need a model for the strings while execution
-- We also need to encode the position of the enzyme and
-- support moving the enzyme back and forth. So we use a 
-- zipper like structure

type BString' = [Maybe Base]

data Context = Context { topL :: BString'   -- Part left to the enzyme.
                       , topC :: Maybe Base -- If this is nothing the execution should be stopped      
                       , topR :: BString'   -- Part right to the enzyme
                         -- And the same for the part the enzyme currently
                         -- not works ob.
                       , botL :: BString'
                       , botC :: Maybe Base  -- This could not be a base. 
                       , botR :: BString'
                         -- We need to keep track of weather the copy mode is on.
                       , copyMode :: Bool 
                         -- These are the steps we still need to do
                       , enzyme :: Enzyme
                       , residues :: [BString]
                       }
                       deriving (Show, Read)

-- a little helper, start at position with enzyme over BString
initContext :: Int -> Enzyme -> BString -> Context
initContext pos enz str =
    let (l, r) = splitAt pos (fmap Just str) 
        bind = bindingBase enz
        cmd = if isPyrimidin bind then PyR else PuR
        context = Context (reverse l) (head r) (tail r) [] Nothing [] False [cmd] []
        context' = execute context
    in if topC context' == Nothing
       then initContext 0 enz str
       else context' { enzyme = enz }

implErrorTopC = error "Implementation error, topC == Nothing."

apply :: EnzymeCommand -> Context -> Context
apply Cut context = context { topR = []
                            , botR = []
                            , residues = residues context ++ _split (topR context) ++ _split (reverse (botR context))
                            }
apply Del context 
    | null . topR $ context = context { topC = Nothing }
    | topC context == Nothing = implErrorTopC
    | otherwise = context { topC = head . topR $ context
                          , topR = tail . topR $ context
                          } -- could be improved with patter matching?
apply MoS context = context { topC = botC context
                            , botC = topC context
                            , topR = botL context
                            , topL = botR context
                            , botL = topR context
                            , botR = topL context
                            } 
apply MoR cont
    | null . topR $ cont = cont { topC = Nothing }
    | otherwise = let context = applyCopyMode cont 
                  in context { topL = topC context : topL context
                             , topC = head . topR $ context
                             , topR = tail . topR $ context -- could be improved?
                             , botL = botC context : botL context
                             , botC = if botR context == [] then Nothing else head . botR $ context
                             , botR = if botR context == [] then [] else tail . botR $ context
                             }
apply MoL cont
    | null . topL $ cont = cont { topC = Nothing }
    | otherwise = let context = applyCopyMode cont
                  in context { topL = tail . topL $ context
                             , topC = head . topL $ context
                             , topR = topC context : topR context -- could be improved?
                             , botL = if botL context == [] then [] else tail . botL $ context
                             , botC = if botL context == [] then Nothing else head . botL $ context
                             , botR = botC context : botR context
                             }
apply COn context = context { copyMode = True } 
apply COf context = context { copyMode = False } 
apply InA context = _insert A context
apply InC context = _insert C context
apply InG context = _insert G context
apply InT context = _insert T context
apply PyR context = applyCopyMode $ _search MoR isPyrimidin context
apply PyL context = applyCopyMode $ _search MoL isPyrimidin context
apply PuR context = applyCopyMode $ _search MoR isPurin context
apply PuL context = applyCopyMode $ _search MoL isPurin context

_insert a context = applyCopyMode $ context { topR = Just a : topR context }

_search :: EnzymeCommand -> (Base -> Bool) -> Context -> Context
_search com test context =
    let context' = apply com context
    in if topC context' == Nothing
       then context'
       else if test . fromJust . topC $ context
            then context
            else _search com test context'

_split str = fmap (fmap fromJust) (splitWhen (Nothing ==) str)

fromJust (Just x) = x

applyCopyMode :: Context -> Context
applyCopyMode context
    | topC context == Nothing = implErrorTopC
    | not $ copyMode context = context
    | otherwise = context { botC = Just . complement . fromJust . topC $ context }

execute = stripBStrings . execute
       
execute' context
    | topC context == Nothing = context
    | null . enzyme $ context = context
    | otherwise = 
        let (e:es) = enzyme context
            context' = apply e context
        in execute' $ context' { enzyme = es } 

stripBStrings context =  filter (not . null) $ 
    _split (reverse (topL context) ++ [topC context] ++ topR context)
    ++ _split (reverse (botR context) ++ [botC context] ++ botL context)
    ++ residues context

-- NOw the last trick: a BString can be converted to an enzyme.

bStringToEnzymes :: BString -> [Enzyme]
bStringToEnzymes str = 
    let paired   = toPairs str
        splitted = splitWhen ((A,A) ==) paired
    in fmap (fmap enzMap) splitted
    where toPairs []       = []
          toPairs (_:[])   = []
          toPairs (a:b:bs) = (a,b) : toPairs bs  
          
          enzMap (A,C) = Cut
          enzMap (A,G) = Del
          enzMap (A,T) = MoS
          enzMap (C,A) = MoR
          enzMap (C,C) = MoL
          enzMap (C,G) = COn
          enzMap (C,T) = COf
          enzMap (G,A) = InA
          enzMap (G,C) = InC
          enzMap (G,G) = InG
          enzMap (G,T) = InT
          enzMap (T,A) = PyR
          enzMap (T,C) = PuR
          enzMap (T,G) = PyL
          enzMap (T,T) = PuL
