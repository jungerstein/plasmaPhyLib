from numpy import cos, sin, sqrt, zeros, arccos

pi = 3.14159265358979323846

def calbzero(valfven, rhoZero):
    res = valfven * (rhoZero**0.5)
    return res

def calcpolarity(c, va, theta, rhoZero, mode):
    thetaRad = theta * pi / 180
    mu = cos(thetaRad)

    res = zeros(7)
    rho_ = 0
    vPara_ = 1
    vPerp_ = 2
    v3_ = 3
    BPara_ = 4
    BPerp_ = 5
    BZero_ = 6
    if (mode == 'fast' or mode == 'slow'):
        vFactor = c / (c**2 - mu**2 * va**2) 
        vParaFactor = c**2 * mu - mu * va**2
        vPerpFactor = sin(arccos(mu)) * c**2
        vPara = vFactor * vParaFactor 
        vPerp = vFactor * vPerpFactor 
        vAbs = sqrt(vPara**2 + vPerp**2)
        epsilon = 1.0 / vAbs # So to make that |delta V| = 1.
        BFactor = vFactor * c * va * sqrt(rhoZero)
        BParaFactor = 1 - mu**2
        BPerpFactor = -mu * sin(arccos(mu))
        BPara = BFactor * BParaFactor
        BPerp = BFactor * BPerpFactor
        res[rho_] = rhoZero * epsilon
        res[vPara_] = vPara * epsilon
        res[vPerp_] = vPerp * epsilon
        res[BPara_] = BPara * epsilon
        res[BPerp_] = BPerp * epsilon
    if (mode == 'alfven'):
        res[v3_] = 1
        res[B3_] = - mu / abs(mu) * sqrt(rhoZero)
    return res

def outputpolar(delta):
    print('Delta Rho: ', delta[0])
    print('Delta v (//, Perp, 3rd): ', delta[1], delta[2], delta[3])
    print('Delta B (//, Perp, 3rd): ', delta[4], delta[5], delta[6])

def outputpressure(pr):
    print('d P_th: ', pr[0])
    print('d P_B: ', pr[1])

''' 
Computes pressures. 
Returns an array of 2: 
  1. Thermal. 
  2. Magnetic, 1st order. 
'''

def pressure(cs, BZero, delta):
  res = zeros(2)
  deltaRho = delta[0]
  deltaP = deltaRho * cs**2
  res[0] = deltaP
  deltaBPara = delta[4]
  res[1] = BZero * deltaBPara
  return res

def wavespeed(cs, va, theta, mode):
  thetaRad = theta * pi / 180 
  cosTheta = cos(thetaRad)
  rootPart = (va**2 + cs**2)**2 - 4 * cosTheta**2 * va**2 * cs**2 
  if (mode == 'fast'):
    return sqrt(((va**2 + cs**2) + sqrt(rootPart))/2)
  if (mode == 'slow'):
    return sqrt(((va**2 + cs**2) - sqrt(rootPart))/2)
  if (mode == 'alfven'):
    return abs(cosTheta) * va

'''
The Interface for User's Calculation of polarity relations. 
'''

def polarity(cs, va, theta, rhoZero):
    B = calbzero(va, rhoZero)
    print('****** FAST WAVE ******')
    c = wavespeed(cs, va, theta, 'fast')
    delta = calcpolarity(c, va, theta, rhoZero, 'fast')
    druck = pressure(cs, B, delta)
    outputpolar(delta)
    outputpressure(druck)
    print('****** SLOW WAVE ******')
    c = wavespeed(cs, va, theta, 'slow')
    delta = calcpolarity(c, va, theta, rhoZero, 'slow')
    druck = pressure(cs, B, delta)
    outputpolar(delta)
    outputpressure(druck)

polarity(1.0, 2.0, 30.0, 1.0)
