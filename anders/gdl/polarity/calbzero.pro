; calbzero.pro
; Computes BZero. 
function calbzero, valfven, rhoZero
  res = valfven * sqrt(rhoZero)
  return, res
end
