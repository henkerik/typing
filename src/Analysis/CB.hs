module Analysis.CB where

solve = solve' worklist d
    data = 
    edge = 

    worklist = 

    where
        solve' data []                       = data 
        solve' data (start,end):worklistTail = solve' newData worklistTail 
            where
                newData = 


class Analysable a where
    