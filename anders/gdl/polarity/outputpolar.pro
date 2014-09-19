; outputpolar.pro
; Outputs polarity relations. 

pro outputpolar, delta
  print, 'Delta Rho: ', delta(0)
  print, 'Delta v (//, Perp, 3rd): ', delta(1), delta(2), delta(3)
  print, 'Delta B (//, Perp, 3rd): ', delta(4), delta(5), delta(6)
end
