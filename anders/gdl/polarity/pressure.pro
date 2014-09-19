; pressure.pro
; Computes pressures. 
; Returns an array of 2: 
;  1. Thermal. 
;  2. Magnetic, 1st order. 

function pressure, cs, BZero, delta 
  res = dblarr(2)

  deltaRho = delta(0)
  deltaP = deltaRho * cs^2
  res(0) = deltaP

  deltaBPara = delta(4)
  res(1) = BZero * deltaBPara

  return, res
end
