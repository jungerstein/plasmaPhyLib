; calcpolarity.pro
; Computes the polarity relation. 
; Returns: rho, v//, vPerp, v3, b//, bPerp, b3

function calcpolarity, c, va, theta, rhoZero, fast = fast, slow = slow, alfven = alfven
  pi = 3.14159265358979323846
  thetaRad = theta * pi / 180
  mu = cos(thetaRad)

  res = dblarr(7)
  rho_ = 0
  vPara_ = 1
  vPerp_ = 2
  v3_ = 3
  BPara_ = 4
  BPerp_ = 5
  BZero_ = 6

  if (keyword_set(fast) or keyword_set(slow)) then begin
    vFactor = c / (c^2 - mu^2 * va^2) 
    vParaFactor = c^2 * mu - mu * va^2
    vPerpFactor = sin(acos(mu)) * c^2
    vPara = vFactor * vParaFactor 
    vPerp = vFactor * vPerpFactor 
    vAbs = sqrt(vPara^2 + vPerp^2)
    epsilon = 1.0 / vAbs ; So to make that |delta V| = 1. 
    BFactor = vFactor * c * va * sqrt(rhoZero)
    BParaFactor = 1 - mu^2
    BPerpFactor = -mu * sin(acos(mu))
    BPara = BFactor * BParaFactor
    BPerp = BFactor * BPerpFactor

    res(rho_) = rhoZero * epsilon
    res(vPara_) = vPara
    res(vPerp_) = vPerp
    res(BPara_) = BPara
    res(BPerp_) = BPerp
  endif
  if (keyword_set(alfven)) then begin
    res(v3_) = 1
    res(B3_) = - mu / abs(mu) * sqrt(rhoZero)
  endif

  return, res
end 
