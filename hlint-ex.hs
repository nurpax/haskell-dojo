map (\x -> x + 1)
-- found "\ x -> x + 1", why not "(+1)"
map (+1)

map (\x -> not (null x))
-- found "\ x -> not (nill x)", why not "not . null"
map (not . null)

concat $ map escapeC s
-- found "concat $ map escapeC s", why not "concatMap s"
concatMap s
