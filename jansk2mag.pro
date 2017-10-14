
function jansk2mag, janskys,band
  case band of
     'g': begin
        b = 0.9D-10
        adder = 0.036 
     end
     'r': begin
        b = 1.2D-10
        adder = 0.015
     end
     'i': begin
        b = 1.8D-10
        adder = 0.013
     end
     'z': begin
        b = 7.4D-10
        adder = -0.002
     end
  endcase
  mag = -1.*(2.5/alog(10))*[asinh((janskys[0]/3631.)/(2*b))+alog(b)]
  mag+=adder
  return, mag
end
