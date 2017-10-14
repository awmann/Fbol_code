pro run_j9_on_models_2, lambda, spectrum, varspec, models, compvalues, metalrange=metalrange, corr=corr, plot=plot, savespec=savespec,savemetal=savemetal,saveteff=saveteff,savelogg=savelogg,saveweight=saveweight,dup=dup,fwhm=fwhm,hot=hot
;EJH
;20120113
;originally written by Eric Gaidos, but modified by EJH to work on the
;J<9 spectra - guts mostly the same, but modified the way it takes
;              in the data, etc

;usemodel options are phoenix,phoenixgiants,marc

;note that models are in vacuum, so make sure that data are at rest
;(corrected for rvraw) and in vacuum

; 3 December
; fixed bug in spectrum standard deviation calculation
; calculated Teff 95% confidence intervals using delta-chisquared
; (appended at end of output)

  loadct, 39,/silent

  rng = Obj_New('RandomNumberGenerator')
  seed = rng->getrandomnumbers(1,/uniform)
  if n_elements(hot) eq 0 then hot = 0
  c = 2.99792458e8              ;speed of light in m/s
  tmin = 2800.                  ; minimum model teff to consider
  tmax = 5100.                  ; maximum ""
  if n_elements(dup) gt 0 then begin
     tmin = 600
     tmax = 3500
  endif
  loggmin = 4.5
  loggmax = 5.0
  if hot eq 1 then begin
     loggmin = 4.0
     loggmax = 5.0
     tmin = 5100
     tmax = 6500
  endif
  lambdamin = 2000              ;5263.7803         ; minimum wavelenght to consider 5263.7803, 10480.009 (derived from general case)
  lambdamax = 30000             ;24000.            ; maximum
  radius = 7.                   ; radius of fitting of cross-correlation function to get exact peak [pixels]
  if n_elements(metalrange) le 1 or finite(metalrange[0]) eq 0 or finite(metalrange[1]) eq 0 then metalrange=[-1.5,0.6]
  if n_elements(plot) eq 0 then plot = 0
  metalmin = metalrange[0]
  metalmax = metalrange[1]
  if n_elements(fwhm) eq 0 then fwhm = 7000./1000.                                  ; approximate resolution of spectrum
  if n_elements(corr) le 1 then corr = [448., 0.8858] ;[573.57,0.852]

  modelspectra = models.spectrum
  modspec = modelspectra*0.
  modelheader = models.header
  lambda_m = sxpar(modelheader,'LAMBDA_0') + findgen(sxpar(modelheader,'NLAMBDA'))*sxpar(modelheader,'D_LAMBDA')
  teff = models.teff
  logg = abs(models.logg)
  metal = models.metal
  fbol = models.fbol
  whichmodels = where((teff le tmax) and (teff ge tmin) and (logg ge loggmin) and (logg le loggmax) and (metal ge metalmin) and (metal lt metalmax))

  ;;whichmodels = where((teff le tmax) and (teff ge 3000) and (logg ge 4.0) and (logg le loggmax) and (metal gt -1.0) and (metal lt 0.5)) ;; remove later


  nmodel = sxpar(modelheader,'NMODEL')
  chisq = make_array(nmodel,value=1.0e35)
  lambdashift = make_array(nmodel, value=0.)
  sigma = sqrt(varspec*(varspec gt 0.) + 100*(varspec le 0.))
  diff = dblarr(n_elements(lambda))-1
  for i=0.0,1.0*n_elements(lambda)-2.0 do diff[i] = lambda[i+1]-lambda[i]
  delta_lambda = median(diff)
  subset = where((lambda ge lambdamin) and (lambda le lambdamax)) ; work on only range of lambda

  if subset[0] ne -1 then begin
     lambda = lambda[subset]
     spectrum = spectrum[subset]
     sigma = sigma[subset]
  endif

  if n_elements(filt) eq 0 then begin ; filtering of difficult areas
     filt = 1.0+dblarr(n_elements(lambda))
     ;;filt = filt[subset]
     if max(lambda) lt 1d4 then begin
        filt = filt*((lambda gt 5500.))                     
        filt = filt*((lambda lt 5587.) + (lambda gt 5605.)) 
        filt = filt*((lambda lt 5801.) + (lambda gt 5852.)) ; further filtering of difficult areas
        filt = filt*((lambda lt 6056.) + (lambda gt 6153.)) ; further filtering of difficult areas
        filt = filt*((lambda lt 6400.) + (lambda gt 6600.)) ; further filtering of difficult areas
        filt = filt*((lambda lt 6915.) + (lambda gt 7020.)) ; further filtering of difficult areas
        filt = filt*((lambda lt 7580.) + (lambda gt 7700.)) ; further filtering of difficult areas
        filt = filt*((lambda lt 8450.) + (lambda gt 8550.)) ; further filtering of difficult areas
        filt = filt*((lambda lt 9300.))                    
     endif else begin ;; 
        filt = filt*(lambda gt 10000.)
        filt = filt*((lambda lt 13300.) + (lambda gt 14300.)) ;; telluric   
        filt = filt*((lambda lt 18000.) + (lambda gt 19500.)) 
        filt = filt*(lambda lt 24000.)
     endelse
     if hot eq 1 then begin
        filt = 1.0+dblarr(n_elements(lambda))
        ;;filt = filt[subset]
        filt = filt*((lambda gt 5500.))                     
        filt = filt*((lambda lt 7580.) + (lambda gt 7700.)) 
        filt = filt*((lambda lt 9300.) + (lambda gt 10000.))
        filt = filt*((lambda lt 13300.) + (lambda gt 14300.)) ;; telluric   
        filt = filt*((lambda lt 18000.) + (lambda gt 19500.)) 
        filt = filt*(lambda lt 24000.)
     endif
  endif 

  nlam = n_elements(where(filt gt 0.))
  p = 1.
  delta = 5.*nlam               ; 
  while (p gt 0.95) do begin
     p = igamma((nlam-3.)/2.,delta/2.)
     delta = delta - 1.
  endwhile

  meanspec = mean(spectrum,/nan)
  spectrum = spectrum/meanspec
  sigma = sigma/meanspec

  bestchisq = 1d40
  for iwhich = 0,n_elements(whichmodels)-1 do begin
     imodel = whichmodels[iwhich]
     model_teffs = teff[imodel]
     model_loggs = logg[imodel]
     model_fehs = metal[imodel]
     specmodel = modelspectra[imodel,*]                               ; extract model spectrum from matrix
     specfold = gaussfold(lambda_m,modelspectra[imodel,*],fwhm)       ; convolve to expected resolution
     interspec = interpol(specfold,lambda_m,lambda)                   ; interpolate at observed wavelengths
     interspec = interspec/mean(interspec,/nan)                       ; normalize
     goodspec = where(finite(interspec),cnt)                          ; extract good parts of spectra
     pixshift = ccpeak(interspec[goodspec],spectrum[goodspec],radius) ; calculate shift from cross-correlation
     ldummy = lambda_m + pixshift*delta_lambda                        ; shift wavelength
     interspec = interpol(specfold,ldummy,lambda)                     ; re-interpolate
     interspec = interspec/mean(interspec,/nan)                       ; normalize
     badspec = where(~finite(interspec),cnt)                          ; find bad parts of model and set to 1
     if (cnt gt 0) then interspec[badspec] = 1. 
     modspec[imodel,subset] = interspec          ; save model spectrum
     lambdashift[imodel] = pixshift*delta_lambda ; save shift
     r = spectrum/interspec                      ; take ratio
     wt = interspec/sigma*filt                   ; calculate weigting
     if (cnt gt 0) then wt[badspec] = 0.         ; set weighting to zero in areas where model not calculated
     chisq[imodel] = total(((r-mean(r))*wt)^2)   ; calculation chi-squared
     if (chisq[imodel] lt bestchisq) and plot eq 1 then begin
        c1 = cgcolor('black')
        c2 = cgcolor('red')
        c3 = cgcolor('blue')
        charsize = 3.5
        charthick = 2.0
        plot,[0],[0],yrange=[0,3], xrange=[4000,24000],/xstyle,/ystyle,ytickn=strarr(10)+' ',xtitle='Wavelength (microns)',charsize=charsize,xtickn=['0.5','1.0','1.5','2.0'],charthick=charthick,color=cgcolor('black'),background=cgcolor('grey'),xthick=5,ythick=5
        oplot, lambda, spectrum+0.75, thick=3, color=cgcolor('black')
        oplot, lambda, interspec+0.75, color=c2,thick=3
        ;;oplot, lambda, wt/max(wt)+1, color=100
        oplot, lambda, ((r-mean(r))*wt)^2/1000000., color=c3,thick=3 ;;/max(((r-mean(r))*wt)^2)
        bestchisq = chisq[imodel]
        xyouts,10000,1.45,'Teff = '+string(model_teffs,format="(I4)")+' K',charsize=charsize/1.25,charthick=charthick,color=c1
        xyouts,10000,1.33,'log(g) = '+string(model_loggs,format="(D3.1)"),charsize=charsize/1.25,charthick=charthick,color=c1
        xyouts,10000,1.21,'[Fe/H] = '+string(model_fehs,format="(D4.1)"),charsize=charsize/1.25,charthick=charthick,color=c1
        plotsym,0,/fill
        legend,['Spectrum','Model','Weighted Residuals'],color=[c1,c2,c3],psym=[8,8,8],charsize=charsize,charthick=charthick,/top,/right,thick=3,textcolor=[c1,c2,c3],box=0
        ;;wait,0.1
     endif
  endfor
  sortindex = sort(chisq)
  minchisq = min(chisq,index,/nan)
  ;;nbest = 7
  minbest = 7                                           ; minimum number of models from which to construct interpolation
  nbest = fix(total(chisq lt minchisq*(1.+delta/nlam))) ; condider only those models with chiseq < 1.5* minchisq
  if nbest lt minbest then nbest = minbest
  
  
  nmonte = long(10000)          ; number of interpolated spectra to examine
  t = make_array(nmonte)
  lg = t
  met = t
  fb = t
  chi2mc = double(t)
  for n = 0d0,1d0*nmonte-1d0 do begin   ; Monte carlo loop
     ii = 0
     jj = 0
     kk = 0
     while ii eq jj or ii eq kk or jj eq kk do begin
        ii = sortindex[fix(nbest*randomu(seed))]
        jj = sortindex[fix(nbest*randomu(seed))]
        kk = sortindex[fix(nbest*randomu(seed))]
     endwhile
     x = [randomu(seed),randomu(seed)]
     x = x[sort(x)]
     spec1 = modspec[ii,subset] ; extract first spectrum
     spec2 = modspec[jj,subset] ; extract second
     spec3 = modspec[kk,subset] ; extract third
     fitspec = x[0]*spec1/mean(spec1) + (x[1]-x[0])*spec2/mean(spec2) + (1.-x[1])*spec3/mean(spec3) ; ternary mixture

     ;;spec1 = modspec[ii,subset] ; extract first spectrum
     ;;spec1 = spec1/mean(spec1)
     ;;spec2 = modspec[jj,subset] ; extract second
     ;;spec2 = spec2/mean(spec2)     
     ;;fitspec = x*spec1 + (1.-x)*spec2 ; generate weighted mixture
     r = spectrum/fitspec             ; calculate ratio of actual to weighted spectrum
     rerr = sigma/fitspec             ; normalized errors
     wt = 1./rerr*filt                ; weighting (actually, square root of)
     if (cnt gt 0) then wt[badspec] = 0.
     chi2mc[n] = total(((r-mean(r))*wt)^2)  ; calculate chi^2
     t[n] = x[0]*teff[ii] + (x[1]-x[0])*teff[jj] + (1.-x[1])*teff[kk] ; calculate weighted teff
     met[n] = x[0]*metal[ii] + (x[1]-x[0])*metal[jj] +(1.-x[1])*metal[kk]           ; calculate weighted metal
     lg[n] = x[0]*logg[ii] + (x[1]-x[0])*logg[jj] +(1.-x[1])*logg[kk]              ; calculate weighted metal
     fb[n] = x[0]*fbol[ii] + (x[1]-x[0])*fbol[jj] +(1.-x[1])*fbol[kk]
     if chi2mc[n] le min(chi2mc[where(chi2mc gt 0)]) then begin
        savespec = fitspec
        saveteff = [teff[ii],teff[jj],teff[kk]]
        savemetal = [metal[ii],metal[jj],metal[kk]]
        savelogg = [logg[ii],logg[jj],logg[kk]]
        saveweight = x
     endif
  endfor
  ;;t = corr[0]+t*corr[1]

  t = t[sort(chi2mc)]
  met = met[sort(chi2mc)]
  lg = lg[sort(chi2mc)]
  fb = fb[sort(chi2mc)]
  st = t*0.
  chi2mc = chi2mc[sort(chi2mc)]
  whichmc = where(chi2mc lt (chi2mc[0] + delta)) ; choose models within 95% confidence inteval
  minteff = min(t[whichmc])                      ; 95% confidence interval
  maxteff = max(t[whichmc])
  for n = 1,nmonte-1 do begin
     st[n] = stddev(t[0:n])     ; standard deviation is maximum value (conservative)
  endfor
  sigteff = max(st)

  whichmc = where(chi2mc lt min(chi2mc) + delta)    ; choose models within 95% confidence inteval
  sigteff = 0.25*(max(t[whichmc]) - min(t[whichmc])) ; standard deviation as 1/4th of 95% confidence range

  compvalues=  [minchisq/nlam, fb[0], lg[0], met[0], lambdashift[index], chi2mc[0]/nlam, t[0], sigteff, minteff, maxteff]


end

