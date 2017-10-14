
PRO get_flux,struct,system,band,lambda,spec,err,flux,flux_err,fwhm
  COMPILE_OPT idl2, HIDDEN
  if system eq 'Eggen' and band eq 'V' then system = 'Johnson'
  if system eq 'Cousins' and band eq 'U' then system = 'Johnson'
  if system eq 'Cousins' and band eq 'B' then system = 'Johnson'
  if system eq 'Cousins' and band eq 'V' then system = 'Johnson'
  if system eq 'Straizys' then system = 'Vilnius'
  if system eq 'Cape' and (band eq 'V' or band eq 'B') then system = 'Johnson'
  if system eq 'Johnson' and band eq 'I' then begin ;; this system doesnt work
     flux = -99
     flux_err = -99
  endif else begin
     nmonte = 200
     l = where(STRLOWCASE(struct.system) eq STRLOWCASE(system) and STRLOWCASE(struct.band) eq STRLOWCASE(band))
     if l[0] eq -1 then begin
        ;;print,'Warning, missing '+system + ' '+band
        fwhm = 0
        flux = -10
        flux_err = 99
     endif else begin
        flambda = *struct[l].lambda
        ftrans = *struct[l].trans
        gg = where(flambda gt 0 and ftrans gt 0)
        flambda = flambda[gg]
        ftrans = ftrans[gg]
        sp = interpol(spec,lambda,flambda)
        er = interpol(err,lambda,flambda)
        flux = integral(flambda,sp*ftrans)/integral(flambda,ftrans) 
        tester = dblarr(nmonte)
        for i = 0,nmonte-1 do begin 
           sp_t = sp+er*randomn(seed,n_elements(sp)) 
           tester[i] = integral(flambda,sp_t*ftrans)/integral(flambda,ftrans)
        endfor
        flux_err = stdev(tester)
        weff = integral(flambda,ftrans)/max(ftrans)
        fwhm = weff/2.
        ;; is Weff = FWHM?
        ;;result = gaussfit(flambda,ftrans,A,nterms=3)
        ;;fwhm = a[2]
        if system eq 'Cousins' and band eq 'I' then flux/=0.97 ;; this might be backwards, I think it's right.
     endelse
  endelse
END
