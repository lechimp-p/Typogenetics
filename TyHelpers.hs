module TyHelpers 
( showBString
, showContext
)
where

import TyGen

showBString str = concat . fmap show $ str

showBString' str = concatMap mapperBString' str

mapperBString' Nothing = " "
mapperBString' (Just b) = show b 

showContext :: Context -> String
showContext context = 
    let lenL = max (length . topL $ context) (length . botL $ context)
        lenR = max (length . topR $ context) (length . botR $ context)
    in  showBString' (reverse (take lenL (topL context ++ repeat Nothing))) 
            ++ "|" ++ mapperBString' (topC context) ++ "|" 
            ++ showBString' (take lenR (topR context ++ repeat Nothing)) 
        ++ "\n" ++  
        showBString' (reverse (take lenL (botL context ++ repeat Nothing)))
            ++ "|" ++ mapperBString' (botC context) ++ "|" 
            ++ showBString' (take lenR (botR context ++ repeat Nothing)) 



