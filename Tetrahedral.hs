import Matrix

-------------------- an expample usage -------------------- 
-- Now we think of
-- * Symmetry Group S_4 
-- * Alternation Group A_4 
-- A_4 is generated just by a & b
-- S_4 is generated just by a, b & z

-- the Matrix is like

-- [x]   [0 0 1][x]
-- [y] = [1 0 0][y]
-- [z]   [0 1 0][z]

-- here x,y or z axis implies each axis of 3 180˚rotations 
-----------------------------------------------------------
e = aaa

a = Mat [
    [ 0, 0, 1],
    [ 1, 0, 0],
    [ 0, 1, 0]]

b = Mat [
    [ 0, 0, 1],
    [-1, 0, 0],
    [ 0,-1, 0]]

c   = b*b*a*a
d   = a*a*b*b
aa  = a*a
bb  = b*b
cc  = c*c 
dd  = d*d
h   = b*a*a 
i   = a*b*a
j   = a*a*b


z = Mat [
    [ 0, 0,-1],
    [ 0,-1, 0],
    [-1, 0, 0]]

za = z * a
zb = z * b
zc = z * c
zd = z * d
zaa = z * aa
zbb = z * bb
zcc = z * cc
zdd = z * dd
zh = z * h
zi = z * i
zj = z * j


ab = a * b
ac = a * c
ad = a * d
ba = b * a
bc = b * c
bd = b * d
ca = c * a
cb = c * b
cd = c * d
da = d * a
db = d * b
dc = d * c


aaa = aa * a
aab = aa * b
aac = aa * c
aad = aa * d
aba = ab * a
abb = ab * b
abc = ab * c
abd = ab * d
aca = ac * a
acb = ac * b
acc = ac * c
acd = ac * d
ada = ad * a
adb = ad * b 
adc = ad * c
add = ad * d
baa = ba * a
bab = ba * b
bac = ba * c
bad = ba * d
bba = bb * a
bbb = bb * b
bbc = bb * c
bbd = bb * d
bca = bc * a
bcb = bc * b
bcc = bc * c
bcd = bc * d
bda = bd * a
bdb = bd * b 
bdc = bd * c
bdd = bd * d
caa = ca * a
cab = ca * b
cac = ca * c
cad = ca * d
cba = cb * a
cbb = cb * b
cbc = cb * c
cbd = cb * d
cca = cc * a
ccb = cc * b
ccc = cc * c
ccd = cc * d
cda = cd * a
cdb = cd * b 
cdc = cd * c
cdd = cd * d
daa = da * a
dab = da * b
dac = da * c
dad = da * d
dba = db * a
dbb = db * b
dbc = db * c
dbd = db * d
dca = dc * a
dcb = dc * b
dcc = dc * c
dcd = dc * d
dda = dd * a
ddb = dd * b 
ddc = dd * c
ddd = dd * d

-- ab - ba 
-- [0 -2  0]
-- [0  0  2]
-- [0  0  0]
--
--   = [0 -2  0  0  0  2  0  0  0]
--  
-- a = [0  0  1  1  0  0  0  1  0]


