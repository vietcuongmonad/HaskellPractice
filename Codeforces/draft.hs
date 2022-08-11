{- This code was written by
Russell Emerine - linguist,
mathematician, coder,
musician, and metalhead. -}
main :: IO ()
main=interact $ unlines . map (p . w) . tail . words

p b | b = "red" | 1>0 ="cyan"

w l=elem '0'l&&sum[read[d]|d<-l]`rem`3<1&&(1<0)#l
False#('0':t)=True#t
b#(c:t)|elem c['0','2'..'8']=1>0|1>0=b#t
_#_=1<0