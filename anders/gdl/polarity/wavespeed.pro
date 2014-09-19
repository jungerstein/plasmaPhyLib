; wavespeed.pro 
; Computes wave phase speeds. 
; Input: cs, va, theta, slow, fast, alfven
; Slow, Fast, Alfven are logic variables. Assign one of them. 
; Theta in degrees. 
; Returns: Wave phase speed, in the same unit as cs and va. 

function wavespeed, cs, va, theta, fast = fast, slow = slow, alfven = alfven
  pi = 3.14159265358979323846
  thetaRad = theta * pi / 180 
  cosTheta = cos(thetaRad)
  rootPart = (va^2 + cs^2)^2 - 4 * cosTheta^2 * va^2 * cs^2 
  if (keyword_set(fast)) then begin
    return, sqrt(((va^2 + cs^2) + sqrt(rootPart))/2)
  endif
  if (keyword_set(slow)) then begin
    return, sqrt(((va^2 + cs^2) - sqrt(rootPart))/2)
  endif
  if (keyword_set(alfven)) then begin
    return, abs(cosTheta) * va
  endif
end 
