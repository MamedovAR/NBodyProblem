replace xs a n = (take n xs) ++ [a] ++ (drop (n+1) xs)

data Node = NoneNode | Node {
    m :: Double,
    m_pos :: [Double],
    momentum :: [Double],
    child :: [Node],
    s :: Double,
    relpos :: [Double]
} deriving (Show,Read)

_init :: Double -> Double -> Double -> Node
_init k x y = Node {m=k,m_pos=[x,y],momentum=[0,0],child=[],s=1,relpos=[x/k,y/k]}

cutoff_dist :: Double
cutoff_dist = 0.002

dist :: Node -> Node -> Double
dist n1 n2 = sqrt((((relpos n1)!!0)-((relpos n2)!!0))^2+(((relpos n1)!!1)-((relpos n2)!!1))^2)

force_on :: Node -> Node -> [Double]
force_on n1 n2 = if d<cutoff_dist then [0,0] else map (\x -> x*((m n1)*(m n2))^3) $ zipWith (-) (relpos n1) (relpos n2)
    where d=dist n1 n2

_subdivide :: Node -> Int -> (Int,Node)
_subdivide n i = if (r)<1 then (0,Node{m=m n,m_pos=m_pos n,momentum=momentum n,child=child n,s=s n,relpos=replace (relpos n) r i}) else (1,Node{m=m n,m_pos=m_pos n,momentum=momentum n,child=child n,s=s n,relpos=replace (relpos n) (r-1) i}) 
    where r = 2*((relpos n)!!i)

into_next_quadrant :: Node -> (Int,Node)
into_next_quadrant n = ((fst (_subdivide (n1) 1)) + 2*(fst (_subdivide (n1) 0)),n1)
    where n1 = Node{m=m n,m_pos=m_pos n,momentum=momentum n,child=child n,s=0.5*s n,relpos=relpos n}

isNoneNode :: Node -> Bool
isNoneNode NoneNode = True
isNoneNode _ = False

add body node = do 
    let smallest_quadrant = 0.0001
    if isNoneNode node then $ do
        body 
        else NoneNode    

force_on1 :: Node -> Node -> Double -> [Double]
force_on1 body node theta
    | null $ child node = force_on (node) body
    | s (node) < (dist (node) body)*theta = force_on (node) body
    | otherwise = [sum(force_on1 body c theta) | c <- child node, not $ isNoneNode c]
