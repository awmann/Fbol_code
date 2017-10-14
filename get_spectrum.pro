PRO get_spectrum,r,lambda,spec,err,stis=stis,wein=wein
  
  if n_elements(wein) eq 0 then wein = 1
  COMPILE_OPT idl2, HIDDEN
  temp = *r.fspec
  spec = temp[*,1]
  lambda = temp[*,0]/1d4
  err = sqrt(temp[*,2])

  ;; correction for systematics
  ll = where(lambda lt 0.9 and err gt 0)
  if ll[0] ne -1 then err[ll] = sqrt(err[ll]^2.0+(0.01*spec[ll])^2.0) 
  ll = where(lambda gt 0.9 and err gt 0)
  if ll[0] ne -1 then err[ll] = sqrt(err[ll]^2.0+(0.01*spec[ll])^2.0) 

  guess = [1.0,5.0]
  f = where(lambda gt 2.6)
  if f[0] eq -1 then f = where(lambda gt 2.0 and err ne 0)
  l =  where(err eq 0.0) & if l[0] ne -1 then err[l] = median(err[where(err ne 0.0)])
  result = mpfitfun('MYRJL',lambda[f],spec[f],err[f],guess,/quiet)
  newlambda = generatearray(max(lambda),30.0,100)
  newspec = myrjl(newlambda,result)
  newerr = newlambda*0.0
  
  lambda = [lambda,newlambda]
  spec = [spec,newspec]
  err = [err,newerr]
  
  if stis eq 0 and wein eq 1 then begin
     l = where(lambda gt 0.335)
     lambda = lambda[l]
     spec = spec[l]
     err = err[l]
     f = where(lambda lt 0.4)
     if f[0] eq -1 then begin
        f = where(lambda lt 0.55)
     endif
     guess = [1.0,-1.0]
     result = mpfitfun('MYWEIN',lambda[f],spec[f],err[f],guess,/quiet)
     result = result[*]
     newlambda = generatearray(0.25,min(lambda),100)
     newspec = mywein(newlambda,result) ;/100.0
     newerr = newlambda*0.0
     lambda = [newlambda,lambda]
     spec = [newspec,spec]
     err = [newerr,err]
  endif

  ;; extrapolate errors
  err[where(err le 0)] = (median(err[where(err gt 0)]/spec[where(err gt 0)]))*err[where(err le 0)]
END
