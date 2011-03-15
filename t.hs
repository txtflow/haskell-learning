--T.hs

gs []     = [[]]
gs (x:xs) = ss ++ map (x:) ss
            where ss = gs xs
                  