PRO tester

  restore,'~/Dropbox/Structures/mdallax_bin.dat' 
  bin = mdallax
  restore,'~/Dropbox/Structures/mdallax.dat'
  s = mdallax
  full = [s,bin]
  print,'Small sample:'
  plotter,s,stop=0
  print,'Big sample:'
  plotter,full
  restore,'~/Dropbox/Radii/feiden5.dat'
  plotter,feiden,/f
  stop

END


PRO usco

  restore,'~/Dropbox/Structures/young.dat'
  restore,'~/Dropbox/Radii/phot_systems.dat'
  y = young[where(young.name eq 'PM_I12576+3513W')]
  y.name = 'USCOCTIO5'
  y.cns3 = 'USCOCTIO5'
  y.othername = 'dummy'
  y.ra = am_racnv('15 59 50.506')
  y.dec = am_deccnv('-19 44 37.35')
  add = 1

  restore,'~/Dropbox/Structures/targets6.dat'
  t = targets[where(Targets.name eq 'PM_I05019-0656')]
  y.irtfspec = t.irtfspec
  
  gcal,y[0],phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add


  stop
  
END

PRO nltt33370

  restore,'~/DRopbox/Structures/targets6.dat'
  restore,'~/Dropbox/Structures/mdallax.dat'
  m = mdallax[0]
  querysimbad,'NLTT 33370',ra,dec
  gcirc,2,ra,dec,targets.ra,targets.dec,dist
  if min(dist) gt 10 then stop
  t = targets[where(dist eq min(dist))]
  struct_assign,t,m
  m.othername = 'NLTT\ 33370'
  restore,'~/Dropbox/Radii/phot_systems.dat'
  ;;x = mrdfits('~/Downloads/2MASS1314+1320_merged_0.3sxd_2012apr20.fits',0,header)
  ;;m.irtfspec = ptr_new(x)
  add=1
  gcal,m,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add,oc=1

  wave = finallambda*1d4
  flux = finalspec
  error = finalerr
  stop
  readcol,'g.dat',glambda,gcurve,f='f,f',/silent
  readcol,'r.dat',rlambda,rcurve,f='f,f',/silent
  readcol,'i.dat',ilambda,icurve,f='f,f',/silent
  readcol,'z.dat',zlambda,zcurve,f='f,f',/silent

  janskys=filter(wave,flux,glambda,gcurve,rlambda,rcurve,ilambda,icurve,zlambda,zcurve)
  ;; Cousins R, I
  ll = where(phot_systems.system eq 'Cousins' and phot_systems.band eq 'I')
  adder = [0.0]
  flambda = *phot_systems[ll].lambda
  ftrans = *phot_systems[ll].TRANS
  tmp = interpol(flux,wave/1d4,flambda)
  flx = integral(flambda,tmp*ftrans)/integral(flambda,ftrans) 
  mags = -2.5*(alog10(flx/phot_systems[ll].zp))
  print,'Ic ', mags
  
  mags=jansk2mag(janskys)
  mags= mags-[0.036, 0.015, 0.013, -0.002]
  print,'SDSS i',mags[3]

  stop

END

PRO epic210363145

  restore,'~/Dropbox/Structures/mdallax.dat'
  m = mdallax[0]
  restore,'~/Dropbox/MyStructures/othertargets.dat'
  o = othertargets[where(othertargets.name eq 'EPIC_210363145')]
  struct_assign,o,m
  m.othername = 'dummy'
  
  restore,'~/Dropbox/Radii/phot_systems.dat'
  add=1
  gcal,m,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=1

  ccm_unred,finallambda*1d4,finalspec,0.04,funred
  l = where(finallambda gt 0.33 and finallambda lt 2.4)
  forprint,string(finallambda[l]*1d4,format="(D7.1)")+string(9b)+string(finalspec[l]*1d14,format="(D10.4)")+String(9b)+string(finalerr[l]*1d14,format="(D10.4)")+string(9b)+string(funred[l]*1d14,format="(D10.4)"),textout='epic210363145_spec.dat',/nocomment

  forprint,string(finallambda[l]*1d4,format="(D7.1)")+string(9b)+string(finalspec[l]*1d14,format="(D10.4)")+String(9b)+string(finalerr[l]*1d14,format="(D10.4)")+string(9b)+string(funred[l]*1d14,format="(D10.4)"),textout='epic210363145_spec.dat',/nocomment

  
  finalfbol_err= sqrt(finalfbol_Err^2.+(finalfbol*0.02)^2.)
  ;; now we need to make adjustments to the result based on the
  ;; reddening

  readcol, 'savage_mathis_ext_law.txt',invlam,elvebv,skipline=1,format='(F,F)',/silent
  svlam = 1./invlam[1:*]
  alebv = 3.1+elvebv[1:*]
  nmonte = 1d3
  ebvs = 0.04+0.01*randomn(seed,nmonte)

  bigfbol = dblarr(n_elements(ebvs))
  v = bigfbol
  for j = 0,n_elements(ebvs)-1 do begin
     ccm_unred,finallambda*1d4,finalspec,ebvs[j],funred
     q = where(finallambda ge 3.5)
     funred[q] = finalspec[q]
     bigfbol[j] = integral(finallambda*1d4,funred)

     ;; Johnson B, V
     ll = where(phot_systems.system eq 'Johnson' and phot_systems.band eq 'V')
     flambda = *phot_systems[ll].lambda
     ftrans = *phot_systems[ll].TRANS
     tmp = interpol(finalspec,finallambda,flambda)
     flx = integral(flambda,tmp*ftrans)/integral(flambda,ftrans) 
     v[j] = -2.5*(alog10(flx/phot_systems[ll].zp))   
  endfor
  print,median(v),stdev(v)
  print,median(bigfbol*1d8),sqrt((stdev(bigfbol*1d8))^2.+(finalfbol_err^2.))

  fbol = median(bigfbol*1d8);finalfbol
  fbol_err = sqrt((stdev(bigfbol*1d8))^2.+(finalfbol_err^2.))

  teff = 4870d0;4620;4840                   ;4700d0
  teff_err = 90d0
  theta = (fbol^.5/(teff/2341.)^2.)                                                     ;; formula we always use, but reversed
  theta_err = sqrt((0.5*2341^2.*fbol^(-0.5)/(teff^2.))^2.*fbol_err^2. +$                ;;
                   (2.*2341^2.*fbol^(0.5)/teff^3.)^2.*teff_err^2.)                      ;; simple propogation
  dist = 130.7
  dist_err = 4.8
  theta_rads = (theta/1000./(60*60))*(!pi/180)                                          ;; convert to radians
  theta_Rads_err = (theta_err/1000./(60*60))*(!pi/180)                                  ;; convert to radians
  coeff = (3.0856d13/695500)/2.                                                         ;; km/pc, Rsun/km, 2 for radius instead of diameter
  radius = (dist*theta_rads)*coeff                                                      ;; atan(theta) ~ theta, been tested, it's fine.
  radius_err = radius*sqrt((dist_err/dist)^2.0 + (theta_rads_err/theta_rads)^2.0)       ;; simple propogation
  print,radius,radius_err
  stop


END


PRO epic211351816

  restore,'~/Dropbox/Structures/mdallax.dat'
  m = mdallax[0]
  restore,'~/Dropbox/MyStructures/othertargets.dat'
  o = othertargets[where(othertargets.name eq 'EPIC_211351816')]
  struct_assign,o,m
  m.othername = 'dummy'
  
  restore,'~/Dropbox/Radii/phot_systems.dat'
  add=1
  gcal,m,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=1
 
  stop

END


PRO praesepe

  close,/all
  restore,'~/Dropbox/MyStructures/othertargets.dat'
  epic = [211901114,211822797,211969807,211916756,211970147,211913977,211990866]
  spt = ['M3.5','M0.7','M2.1','M3.3','K3.6','K3.2','F6.2']
  epic = epic[4:6]
  spt = spt[4:6]
  match,'EPIC_'+strtrim(epic,2),othertargets.name,suba,subb
  o = othertargets[subb]
  epic = epic[suba]
  spt = spt[suba]
  restore,'~/Dropbox/Radii/phot_systems.dat'

  restore,'~/Dropbox/Structures/mdallax.dat'
  m = mdallax[0]

  nmonte = 1d3

  names = strarr(n_elements(o))
  bigfbols = dblarr(n_elements(o))
  bigfbols_err = bigfbols
  for i = 0,n_elements(o)-1 do begin
     struct_assign,o[i],m
     print,m.name,m.ra,m.dec
     if m.name eq 'EPIC_211901114' then begin
        mm = mdallax[wherE(mdallax.name eq 'PM_I00118+2259')]
        m.uh22spec_b = mm.uh22spec_b
        m.uh22var_b = mm.uh22var_b
     endif
        
     if strpos(spt[i],'F') ne -1 then hot = 1 else hot = 0
     add = 1
     m.irtffeh = 0.16
     gcal,m,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add,hot=hot,printname=1
     ebv = 0.027+0.004*randomn(seed,nmonte)
     fbols = dblarr(nmonte)
     for j = 0d0,nmonte-1d0 do begin
        ccm_unred,finallambda*1d4,finalspec,ebv[j],funred
        q = where(finallambda ge 3.5)
        funred[q] = finalspec[q]
        fbols[j] = integral(finallambda*1d4,funred)
     endfor
     names[i] = o[i].name
     bigfbols[i] = median(Fbols)*1d8
     bigfbols_err[i] = 1d8*sqrt(stdev(fbols)^2.+(finalfbol_err/1d8)^2.)

     print,names[i]+string(9b)+spt[i]+string(9b)+string(fbols[i],format="(E12.5)")+string(9b)+string(bigfbols_err[i],format="(E12.5)")+string(9b)+strtrim(string(100.*bigfbols_err[i]/bigfbols[i],format="(D6.2)"),2)+'%'
     
  endfor
  ;;forprint,names+string(9b)+spt+string(9b)+string(bigfbols,format="(E12.5)")+string(9b)+string(bigfbols_err,format="(E12.5)"),textout='Praesepe_params.txt',comment = 'Name  Spt  Fbol  Fbol_err'
  close,/all
  stop
END

PRO epic205046529

  cgloadct,0
  !x.margin=[8,1]
  !y.margin=[5,1]
  restore,'~/Dropbox/Structures/mdallax.dat'
  m = mdallax[0]
  restore,'~/Dropbox/MyStructures/othertargets.dat'
  o = othertargets[where(othertargets.name eq 'EPIC_205046529')]
  struct_assign,o,m
  m.othername = 'dummy'
  
  restore,'~/Dropbox/Radii/phot_systems.dat'
  add=1
  gcal,m,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add
  stop
  ccm_unred,finallambda*1d4,finalspec,0.228,funred
  l = where(finallambda gt 0.33 and finallambda lt 2.4)
  ;;forprint,string(finallambda[l]*1d4,format="(D7.1)")+string(9b)+string(finalspec[l]*1d14,format="(D10.4)")+String(9b)+string(finalerr[l]*1d14,format="(D10.4)")+string(9b)+string(funred[l]*1d14,format="(D10.4)"),textout='epic210363145_spec.dat',/nocomment

  finalfbol_err= sqrt(finalfbol_Err^2.+(finalfbol*0.02)^2.)
  ;; now we need to make adjustments to the result based on the
  ;; reddening

                                ;readcol,'Params.txt',ebvs,teffs,chisqs
                                ;l = where(chisqs lt 1)
                                ;ebvs = ebvs[l]
                                ;teffs = teffs[l]
                                ;chisqs = chisqs[l]
                                ;nmonte = n_elements(ebvs)
  
  nmonte = 1d4
  ;tmp = randomn(seed,nmonte)
  ;tmp[where(tmp gt 0)]*=0.21
  ;tmp[where(tmp lt 0)]*=0.08
  ;ebvs = 0.22+tmp
  ebvs = 0.23+0.07*randomn(seed,nmonte)
  ;;ebvs = 0.17+0.07*randomn(seed,nmonte) ;; corresponds to 0.53 +/- 0.24
  ebvs[where(ebvs le 0)] = 0d0
  teff = o.uh22model_Values[6] + 80d0*randomn(seed,nmonte)
  ;;teff = 3540d0 + 70d0*randomn(seed,nmonte)

  ;;ebvs = 0.15+0.06*randomn(seed,nmonte)
  ;;ebvs[where(ebvs le 0)] = 0d0
  ;;teff = 3384d0 + 70d0*randomn(seed,nmonte)

  bigfbol = dblarr(n_elements(ebvs))
  v = bigfbol
  for j = 0d0,1d0*n_elements(ebvs)-1d0 do begin
     ccm_unred,finallambda*1d4,finalspec,ebvs[j],funred
     q = where(finallambda ge 3.5)
     funred[q] = finalspec[q]
     bigfbol[j] = integral(finallambda*1d4,funred)

     ;; Johnson B, V
     ;ll = where(phot_systems.system eq 'Johnson' and phot_systems.band eq 'V')
     ;flambda = *phot_systems[ll].lambda
     ;ftrans = *phot_systems[ll].TRANS
     ;tmp = interpol(finalspec,finallambda,flambda)
     ;flx = integral(flambda,tmp*ftrans)/integral(flambda,ftrans) 
     ;;v[j] = -2.5*(alog10(flx/phot_systems[ll].zp))   
  endfor
  ;;print,median(v),stdev(v)
  print,median(bigfbol*1d8),sqrt((stdev(bigfbol*1d8))^2.+(finalfbol_err^2.))

  fbol = median(bigfbol*1d8);finalfbol
  fbol_err = sqrt((stdev(bigfbol*1d8))^2.+(finalfbol_err^2.))

  ;teff = 3540d0;4620;4840                   ;4700d0
  ;teff_err = 70d0
  theta = (fbol^.5/(teff/2341.)^2.)                                                     ;; formula we always use, but reversed
  ;;theta_err = sqrt((0.5*2341^2.*fbol^(-0.5)/(teff^2.))^2.*fbol_err^2. +$                ;;
  ;;                 (2.*2341^2.*fbol^(0.5)/teff^3.)^2.*teff_err^2.)                      ;; simple propogation
  dist = 145.0 + 15d0*randomn(seed,nmonte)
  theta_rads = (theta/1000./(60*60))*(!pi/180)                                          ;; convert to radians
  ;;theta_Rads_err = (theta_err/1000./(60*60))*(!pi/180)                                  ;; convert to radians
  coeff = (3.0856d13/695500)/2.                                                         ;; km/pc, Rsun/km, 2 for radius instead of diameter
  radius = (dist*theta_rads)*coeff                                                      ;; atan(theta) ~ theta, been tested, it's fine.
  ;;radius_err = radius*sqrt((dist_err/dist)^2.0 + (theta_rads_err/theta_rads)^2.0)       ;; simple propogation
  print,median(radius),stdev(radius)

  set_plot,'x'
  !p.multi=[0,1,1]
  plot,teff,bigfbol,psym=3,/xstyle,/ystyle
  
  stop


END


PRO EPIC205117205

  cgloadct,0
  !x.margin=[8,1]
  !y.margin=[5,1]
  restore,'~/Dropbox/Structures/mdallax.dat'
  m = mdallax[0]
  restore,'~/Dropbox/MyStructures/othertargets.dat'
  o = othertargets[where(othertargets.name eq 'EPIC_205117205')]
  x = mrdfits('EPIC205117205_final.fits',0,header)
  ;; make some edits
  lam = x[*,0]
  sp = x[*,1]
  err = x[*,2]
  sp[where(sp lt 0)] = mean([sp[where(sp lt 0)+1],sp[where(sp lt 0)-1]])
  sp[where(lam gt 1.8)]*=0.96
  ray = where(sp gt 1.2d-14 and lam gt 1.1 and lam lt 1.2)
  sp[ray] = median(sp[[ray[0]-2,ray[0]-1,ray,ray[1]+1,ray[1]+2]])
  ;;plot,lam,sp,xrange=[1.4,1.5],/xstyle,/ystyle
  sp[4094:4097] = 8.1050906e-15
  x[*,1] = sp

  o.irtfspec = ptr_new(x)
  struct_assign,o,m
  m.othername = 'dummy'
  
  restore,'~/Dropbox/Radii/phot_systems.dat'
  add=1
  gcal,m,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add

  ccm_unred,finallambda*1d4,finalspec,0.228,funred
  ;;l = where(finallambda gt 0.33 and finallambda lt 2.4)
  ;;forprint,string(finallambda[l]*1d4,format="(D7.1)")+string(9b)+string(finalspec[l]*1d14,format="(D10.4)")+String(9b)+string(finalerr[l]*1d14,format="(D10.4)")+string(9b)+string(funred[l]*1d14,format="(D10.4)"),textout='epic210363145_spec.dat',/nocomment

  finalfbol_err= sqrt(finalfbol_Err^2.+(finalfbol*0.02)^2.)
  ;; now we need to make adjustments to the result based on the
  ;; reddening

                                ;readcol,'Params.txt',ebvs,teffs,chisqs
                                ;l = where(chisqs lt 1)
                                ;ebvs = ebvs[l]
                                ;teffs = teffs[l]
                                ;chisqs = chisqs[l]
                                ;nmonte = n_elements(ebvs)
  
  nmonte = 1d4
  tmp = randomn(seed,nmonte)
  tmp[where(tmp gt 0)]*=(0.21/3.1)
  tmp[where(tmp lt 0)]*=(0.21/3.1)
  ebvs = (0.75/3.1d0)+tmp
  ;;ebvs = 0.23+0.07*randomn(seed,nmonte)
  ;;ebvs = 0.17+0.07*randomn(seed,nmonte) ;; corresponds to 0.53 +/- 0.24
  ebvs[where(ebvs le 0)] = 0d0
  teff = 3540d0 + 70d0*randomn(seed,nmonte)

  ;;ebvs = 0.15+0.06*randomn(seed,nmonte)
  ;;ebvs[where(ebvs le 0)] = 0d0
  ;;teff = 3384d0 + 70d0*randomn(seed,nmonte)

  bigfbol = dblarr(n_elements(ebvs))
  v = bigfbol
  for j = 0d0,1d0*n_elements(ebvs)-1d0 do begin
     ccm_unred,finallambda*1d4,finalspec,ebvs[j],funred
     q = where(finallambda ge 3.5)
     funred[q] = finalspec[q]
     bigfbol[j] = integral(finallambda*1d4,funred)

     ;; Johnson B, V
     ;ll = where(phot_systems.system eq 'Johnson' and phot_systems.band eq 'V')
     ;flambda = *phot_systems[ll].lambda
     ;ftrans = *phot_systems[ll].TRANS
     ;tmp = interpol(finalspec,finallambda,flambda)
     ;flx = integral(flambda,tmp*ftrans)/integral(flambda,ftrans) 
     ;;v[j] = -2.5*(alog10(flx/phot_systems[ll].zp))   
  endfor
  ;;print,median(v),stdev(v)
  print,median(bigfbol*1d8),sqrt((stdev(bigfbol*1d8))^2.+(finalfbol_err^2.))

  fbol = median(bigfbol*1d8);finalfbol
  fbol_err = sqrt((stdev(bigfbol*1d8))^2.+(finalfbol_err^2.))
  fbol = 2.25d-2
  fbol_err = 0.26d-2
  
  ;teff = 3540d0;4620;4840                   ;4700d0
  ;teff_err = 70d0
  theta = (fbol^.5/(teff/2341.)^2.)                                                     ;; formula we always use, but reversed
  ;;theta_err = sqrt((0.5*2341^2.*fbol^(-0.5)/(teff^2.))^2.*fbol_err^2. +$                ;;
  ;;                 (2.*2341^2.*fbol^(0.5)/teff^3.)^2.*teff_err^2.)                      ;; simple propogation
  dist = 145.0 + 15d0*randomn(seed,nmonte)
  theta_rads = (theta/1000./(60*60))*(!pi/180)                                          ;; convert to radians
  ;;theta_Rads_err = (theta_err/1000./(60*60))*(!pi/180)                                  ;; convert to radians
  coeff = (3.0856d13/695500)/2.                                                         ;; km/pc, Rsun/km, 2 for radius instead of diameter
  radius = (dist*theta_rads)*coeff                                                      ;; atan(theta) ~ theta, been tested, it's fine.
  ;;radius_err = radius*sqrt((dist_err/dist)^2.0 + (theta_rads_err/theta_rads)^2.0)       ;; simple propogation
  print,median(radius),stdev(radius)

  set_plot,'x'
  !p.multi=[0,1,1]
  plot,teff,bigfbol,psym=3,/xstyle,/ystyle
  
  stop


END

PRO young

  add = 2
  set_plot,'x'
  restore,'~/Dropbox/Radii/phot_systems.dat'
  restore,'~/Dropbox/Structures/young.dat'
  ;;young = young[where(young.uh22snr gt 60 and young.irtfsnr gt 60 and young.plx_error/young.plx lt 0.1 and young.plx gt 0 and young.plx_error gt 0)]
  ;;young = young[sort(young.ra)]
  young.irrv = young.irtfrv
  young[where(young.name eq 'TWA_11_C')].othername = 'dummy'
  young[wherE(young.name eq 'J04130515145')].plx = 1./45.5
  young[wherE(young.name eq 'J04130515145')].plx_error = (1./45.5)*(4.5/45.5)

  tmp=*young[wherE(young.name eq 'J04130515145')].irtfspec
  sp = tmp[*,1]
  lam = tmp[*,0]
  sp[where(lam gt 1.3 and lam lt 1.45)] = smooth(sp[where(lam gt 1.3 and lam lt 1.45)],3)
  tmp[*,1] = sp
  young[wherE(young.name eq 'J04130515145')].irtfspec = ptr_new(tmp)


  ;; J22021626-4210329 might need to be re-observed, that weird
  ;; saturation issue came up
  ;; 22060961-0723353 redo this later
  syserr = 60.0
  ;;young[where(young.name eq 'PM_I03332+4615S')].lum = 0.
  ;;young[where(young.cluster eq 'THA' or young.name eq 'PM_I11018-3442')].lum = 0
  print,max(where(young.plx_error/young.plx gt 0.1 and young.plx gt 0 and young.plx_error gt 0))
  for i = 0,2014,1 do begin ;,n_Elements(young)-1 do begin
     if young[i].uh22snr gt 80 and young[i].irtfsnr gt 50 and young[i].plx_error/young[i].plx lt 0.1 and young[i].plx gt 0 and young[i].plx_error gt 0 and young[i].lum le 0.0 and young[i].name ne 'TWA_11B' and young[i].name ne 'G_269-153_A' and young[i].name ne 'GJ_3305AB' and young[i].name ne '20334670-3733443' and young[i].name ne 'J21143354-4213528' and young[i].name ne 'J22021626-4210329' and young[i].name ne 'TWA_34' then begin ;
        ;;if young[i].name eq 'J04130515145' then begin
        print,i
        gcal,young[i],phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=1,long=-1
        fbol = finalfbol
        fbol_err = finalfbol_err
        teff = young[i].uh22model_Values[6]                                          ;; Teff from model fitting
        teff_err = 60.;sqrt(young[i].uh22model_Values[7]^2.+syserr^2.)                   ;; conservative, should this be fixed at 60?
        print,teff,teff_err
        theta = (fbol^.5/(teff/2341.)^2.)                                                  ;; formula we always use, but reversed
        theta_err = sqrt((0.5*2341^2.*fbol^(-0.5)/(teff^2.))^2.*fbol_err^2. +$             ;;
                         (2.*2341^2.*fbol^(0.5)/teff^3.)^2.*teff_err^2.)                   ;; simple propogation
        dist = 1./young[i].plx                                                             ;; plx-> distance
        dist_err = dist*(young[i].plx_error/young[i].plx)                                  ;; simple propogation
        theta_rads = (theta/1000./(60*60))*(!pi/180)                                       ;; convert to radians
        theta_Rads_err = (theta_err/1000./(60*60))*(!pi/180)                               ;; convert to radians
        coeff = (3.0856d13/695500)/2.                                                      ;; km/pc, Rsun/km, 2 for radius instead of diameter
        radius = (dist*theta_rads)*coeff                                                   ;; atan(theta) ~ theta, been tested, it's fine.
        radius_err = radius*sqrt((dist_err/dist)^2.0 + (theta_rads_err/theta_rads)^2.0)    ;; simple propogation
        mk = young[i].k - 5.0*(alog10(dist)-1.)
        print,string(mk,format="(D6.2)")+string(9b)+string(teff,format="(I4)"),radius,radius_err
        young[i].rad = radius
        young[i].rad_err = radius_err
        lum = 4.*!pi*(3.08567758d18*dist)^2.*(fbol*1d-8) / 3.846d33
        fbols = fbol+fbol_err*randomn(seed,1000)
        dists = dist+dist_err*randomn(seed,1000)
        lum_tmp = 4.*!pi*(3.08567758d18*dists)^2.*(fbols*1d-8) / 3.846d33
        young[i].lum = lum
        young[i].lum_err = stdev(lum_tmp)
     endif
  endfor

  stop
  save,filename='~/Dropbox/Structures/young.dat',young
  
END

PRO mdallax,redo=redo,start=start,finish=finish,bin=bin,add=add,stop=stop


  ;;GJ673, PM_I01528-2226
  !p.multi=[0,1,1]
  !x.margin = [8,6]
  !y.margin=[5,2]
  if n_elements(add) eq 0 then add = 1
  if n_elements(bin) eq 0 then bin = 0
  if n_elements(stop) eq 0 then stop = 0

  if bin eq 1 then restore,'~/Dropbox/Structures/mdallax_bin.dat' else restore,'~/Dropbox/Structures/mdallax.dat'
  
  syserr = 60. ;; systematic error to add to temperature error
  restore,'~/Dropbox/Radii/phot_systems.dat'
  tmp = mdallax.cns3
  strreplace,tmp,'NN ','GJ '
  mdallax.cns3 = tmp
  tmp = mdallax.othername
  strreplace,tmp,'Gliese\ NN ','Gliese\ '
  mdallax.othername = tmp
  mdallax = mdallax[sort(mdallax.ra)]

  l = where(mdallax.name eq 'PM_I10196+1952') & if l[0] ne -1 then mdallax[l].irfharr = [0.15,0.15,0.15]
  l = where(mdallax.name eq 'PM_I10196+1952') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3370
  l = where(mdallax.name eq 'PM_I04429+1857') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3680
  l = where(mdallax.name eq 'PM_I16555-0823') & if l[0] ne -1 then mdallax[l].othername = 'Gliese\ 644'
  l = where(mdallax.name eq 'PM_I09319+3619') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3692
  l = where(mdallax.name eq 'PM_I13450+1747') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3828
  l = where(mdallax.name eq 'PM_I16542+1154') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3834
  l = where(mdallax.name eq 'PM_I16570-0420') & if l[0] ne -1 then mdallax[l].othername = '0980019007'
  
  if bin eq 0 then begin
     l = wherE(mdallax.name eq 'PM_I00183+4401') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 15A'
     l = where(mdallax.cns3 eq 'GJ 15A') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 001326'
     l = wherE(mdallax.name eq 'PM_I03361+3118') & if l[0] ne -1 then mdallax[l].othername = ''
     l = wherE(mdallax.name eq 'PM_I13457+1453') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 119850'
     l = wherE(mdallax.name eq 'PM_I13457+1453') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 526'
     l = wherE(mdallax.name eq 'PM_I17578+0441N') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 699'
     l = wherE(mdallax.name eq 'PM_I09143+5241') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 338A'
     l = wherE(mdallax.name eq 'PM_I09143+5241') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 79210'
     l = wherE(mdallax.name eq 'PM_I05314-0340') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 205'
     l = wherE(mdallax.name eq 'PM_I05314-0340') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 36395'
     l = wherE(mdallax.name eq 'PM_I18427+5937N') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 725A'
     l = wherE(mdallax.name eq 'PM_I18427+5937S') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 725B'
     l = where(mdallax.cns3 eq 'GJ 725A') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 173739'
     l = where(mdallax.cns3 eq 'GJ 725B') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 173740'
     l = wherE(mdallax.name eq 'PM_I01186-0052S') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 7895'
     l = where(mdallax.name eq 'PM_I04290+2155') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 28343'
     l = where(mdallax.name eq 'PM_I23318+1956E') & if l[0] ne -1 then mdallax[l].irfharr = [0.03,0.03,0.03]
     l = where(mdallax.name eq 'GJ673') & if l[0] ne -1 then mdallax[l].othername = 'Gliese\ 673'
     l = where(mdallax.name eq 'GJ673') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 673'
     l = where(mdallax.name eq 'GJ673') & if l[0] ne -1 then mdallax[l].plx = 129.86/1d3
     l = where(mdallax.name eq 'GJ673') & if l[0] ne -1 then mdallax[l].plx_error = 0.73/1d3
     l = where(mdallax.name eq 'PM_I06024+4951') & if l[0] ne -1 then mdallax[l].othername = '0980192015'
     l = where(mdallax.name eq 'PM_I06490+3706') & if l[0] ne -1 then mdallax[l].othername = '0980087008'
  endif
  if bin eq 1 then begin
     l = where(mdallax.name eq 'PM_I03047+6144') & if l[0] ne -1 then mdallax[l].othername = 'Gliese\ 3195'
     l = wherE(mdallax.name eq 'PM_I22524+0954') & if l[0] ne -1 then mdallax[l].othername = 'Gliese\ 9801'
     l = wherE(mdallax.name eq 'PM_I22524+0954') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 9801'
     l = wherE(mdallax.name eq 'PM_I01076+2257E') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 53.1 B'
     l = wherE(mdallax.name eq 'PM_I01076+2257E') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 6660'
     l = wherE(mdallax.name eq 'PM_I19321-1119') & if l[0] ne -1 then mdallax[l].othername = 'dummy'
     l = where(mdallax.name eq 'Gl166C') & if l[0] ne -1 then mdallax[l].othername = 'HD\ 026976'
     l = where(mdallax.name eq 'PM_I11055+4331') & if l[0] ne -1 then mdallax[l].othername = 'Gliese\ 412'
     l = where(mdallax.name eq 'PM_I11055+4331') & if l[0] ne -1 then mdallax[l].cns3 = 'GJ 412B'
  endif

  l = where(mdallax.name eq 'PM_I18427+5937S') & if l[0] ne -1 then mdallax[l].uh22model_Values[6] = 3345
  l = where(mdallax.name eq 'PM_I16554-0819') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3279
  l = where(mdallax.name eq 'PM_I22468+4420') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3262
  l = where(mdallax.name eq 'PM_I08526+2818') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3166

  l = where(mdallax.name eq 'PM_I05314-0340') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3801 ;; weird...

  l = where(mdallax.name eq 'PM_I17095+4340') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3284
  l = where(mdallax.name eq 'PM_I23245+5751S') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3764
  l = where(mdallax.name eq 'PM_I03181+3815') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3700
  l = where(mdallax.name eq 'PM_I19396-2645') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3956
  l = where(mdallax.name eq 'PM_I22374+3922') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 4131
  l = where(mdallax.name eq 'PM_I01324-2154') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3641
  l = where(mdallax.name eq 'PM_I00219-3124') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3600
  l = where(mdallax.name eq 'PM_I11421+2642') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3479
  l = where(mdallax.name eq 'PM_I20533+6209') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3791
  l = where(mdallax.name eq 'PM_I20533+6209') & if l[0] ne -1 then mdallax[l].irfharr = [-0.10,-0.10,-0.05]
  l = where(mdallax.name eq 'PM_I07274+0513') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3317
  l = where(mdallax.name eq 'PM_I06461+3233') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3656
  l = where(mdallax.name eq 'PM_I14342-1231') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3211
  l = where(mdallax.name eq 'PM_I20407+1954') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3501
  l = where(mdallax.name eq 'PM_I16581+2544') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3700
  l = where(mdallax.name eq 'PM_I02534+1724') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3377
  l = where(mdallax.name eq 'PM_I04290+2155') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 4124
  l = where(mdallax.name eq 'PM_I23505-0933') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3221
  l = where(mdallax.name eq 'PM_I01432+2750') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3852
  l = where(mdallax.name eq 'PM_I12151+4843') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3900
  l = where(mdallax.name eq 'PM_I13196+3320') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3650
  l = where(mdallax.name eq 'PM_I23182+4617') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3910

  ;;l = where(mdallax.name eq 'PM_I02336+2455') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3190
  ;;l = where(mdallax.name eq 'PM_I18498-2350') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3213
  l = where(mdallax.name eq 'PM_I22532-1415') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3247 ;;3321
  l = where(mdallax.name eq 'PM_I10508+0648') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3238
  l = where(mdallax.name eq 'PM_I17378+1835') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3657
  l = where(mdallax.name eq 'PM_I02441+4913W') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3685 ;3808
  l = where(mdallax.name eq 'PM_I18165+4533') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3785
  l = where(mdallax.name eq 'PM_I07344+6256') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3681
  l = where(mdallax.name eq 'PM_I13299+1022') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3727
  l = where(mdallax.name eq 'PM_I13299+1022') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3727

  l = where(mdallax.name eq 'PM_I02555+2652') & if l[0] ne -1 then begin
     mdallax[l].plx = 42.57/1d3
     mdallax[l].plx_error = 0.84/1d3
     mdallax[l].plx_source = 'VL07'
  endif
  l = where(mdallax.name eq 'PM_I07287-0317') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3448
  l = where(mdallax.name eq 'PM_I04376-1102') & if l[0] ne -1 then mdallax[l].uh22model_values[6] = 3671
  l = where(mdallax.name eq 'PM_I00184+4401') & if l[0] ne -1 then mdallax[l].irfharr = [-0.3,-0.3,-0.3]
  l = where(mdallax.name eq 'PM_I17578+0441N') & if l[0] ne -1 then mdallax[l].irfharr = [-0.4,-0.4,-0.41]


  l = wherE(mdallax.cns3 eq 'GJ 15A') & if l[0] ne -1 then mdallax[l].irfharr = [-0.3,-0.3,-0.3]
  l = wherE(mdallax.cns3 eq 'GJ 15B') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 15B')].irfharr = [-0.3,-0.3,-0.3]
  l = wherE(mdallax.cns3 eq 'GJ 205') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 205')].irfharr = [+0.49,+0.49,+0.49]
  l = wherE(mdallax.cns3 eq 'GJ 380') & if l[0] ne -1 then  mdallax[wherE(mdallax.cns3 eq 'GJ 380')].irfharr = [0.24,0.24,0.24]
  l = wherE(mdallax.cns3 eq 'GJ 526') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 526')].irfharr = [-0.31,-0.31,-0.31]
  l = wherE(mdallax.cns3 eq 'GJ 687') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 687')].irfharr = [0.05,0.05,0.05]
  l = wherE(mdallax.cns3 eq 'GJ 880') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 880')].irfharr = [0.21,0.21,0.21]
  l = wherE(mdallax.cns3 eq 'GJ 699') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 699')].irfharr = [-0.40,-0.40,-0.40]
  l = wherE(mdallax.cns3 eq 'GJ 411') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 411')].irfharr = [-0.38,-0.38,-0.38]
  l = wherE(mdallax.cns3 eq 'GJ 412A') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 412A')].irfharr = [-0.37,-0.37,-0.37]
  l = wherE(mdallax.cns3 eq 'GJ 436') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 436')].irfharr = [0.01,0.01,0.01]
  l = wherE(mdallax.cns3 eq 'GJ 581') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 581')].irfharr = [-0.15,-0.15,-0.15]
  l = wherE(mdallax.cns3 eq 'GJ 725A') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 725A')].irfharr = [-0.23,-0.23,-0.23]
  l = wherE(mdallax.cns3 eq 'GJ 725B') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 725B')].irfharr = [-0.30,-0.30,-0.30]
  l = wherE(mdallax.cns3 eq 'GJ 809') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 809')].irfharr = [-0.06,-0.06,-0.06]
  l = where(mdallax.cns3 eq 'GJ 702B') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 702B')].irfharr = [0.01,0.01,0.01]
  l = where(mdallax.cns3 eq 'GJ 570A') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 570A')].irfharr = [-0.06,-0.06,-0.06]
  l = where(mdallax.cns3 eq 'GJ 820A') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 820A')].irfharr = [-0.27,-0.27,-0.27]
  l = where(mdallax.cns3 eq 'GJ 820B') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 820B')].irfharr = [-0.22,-0.22,-0.22]
  l = where(mdallax.cns3 eq 'GJ 892') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 892')].irfharr = [-0.23,-0.23,-0.23]
  l = wherE(mdallax.cns3 eq 'GJ 887') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 887')].irfharr = [-0.06,-0.06,-0.06]
  l = where(mdallax.cns3 eq 'GJ 105A') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 105A')].irfharr = [-0.28,-0.28,-0.28]
  l = where(mdallax.cns3 eq 'GJ 105B') & if l[0] ne -1 then mdallax[wherE(mdallax.cns3 eq 'GJ 105B')].irfharr = [-0.28,-0.28,-0.28]
  ;; 338 A, B
  l = where(mdallax.name eq 'PM_I09143+5241') & if l[0] ne -1 then mdallax[wherE(mdallax.name eq 'PM_I09143+5241')].irfharr = [-0.01,-0.01,-0.01]
  l = where(mdallax.name eq 'PM_I09144+5241') & if l[0] ne -1 then mdallax[wherE(mdallax.name eq 'PM_I09144+5241')].irfharr = [-0.04,-0.04,-0.04]

  tmp = strtrim(mdallax.cns3,2)
  strreplace,tmp,'Gl','GJ'
  strreplace,tmp,'  ',' '
  strreplace,tmp,'  ',' '
  strreplace,tmp,' B','B'
  strreplace,tmp,' A','A'
  strreplace,tmp,'Gliese','Gliese\ '
  mdallax.cns3 = tmp
  ;; handled in build_mdallax
  ;; PM_I01024-2136W I can't even figure out what this is...
  ;; PM_I22524+0954 is higher-order system
  ;; PM_I02441+4913W has basically no photometry
  ;; PM_I19216+2052 (GJ 1235) has 0 optical photometry :(
  ;; PM_I14010-0239 has shape issues, re-observe in NIR?
  ;; PM_I16090+5256 has massive disagreement between optical and NIR re-observe?
  ;; PM_I16578+4722E (GJ 649.1B) is mislabeled as 649.1A, also the optical spectrum is for the A star (which is a K dwarf). NIR spectrum looks fine.
  ;; PM_I18353+4544 might need to be reovbserved (NIR) (par angle?)
  ;; PM_I19396-2645 UH88 spectrum has some serious shit on the red end, needs new obs

  ;; blue spectrum on PM_I17176+5224 looks insane
  ;; PM_I19539+4424W should be revisited because it's very cool, but agreement issues
  ;; PM_I19539+4424E same deal as W
  ;; PM_I22467+1210 massive bright nearby star (8"), contaminated photometry?
  ;; PM_I23318+1956E is active, probably why it's overluminous

  ;;BIN:
  ;; LSPM_J0045+0015N was observed poorly in the NIR, is not merged,
  ;; and has some photometry issues (maybe). Probably not worth the efford
  ;; PM_I20407+1954 might be savable, negative B flux

  ;; PM_I09151+2321S has some spectral/target confusion issues
  ;; LSPM_J0816+5704 not worth trouble
  ;; Gl768.1B removed for lazyness, could be merged and added later? 
  ;; PM_I10304+5559 really high rchisq, not important
  ;; too lazy for LSPM_J2231+4509


  ;; PM_I10564+0700 might be trouble
  ;; this list is more up to date than the one below
  ;; take a look at PM_I17578+4635

  counter = 0
  if n_elements(start) eq 0 then start = 0 
  if n_elements(finish) eq 0 then finish = n_elements(mdallax)
  pnts = intarr(1)
  rchisqs = dblarr(1)
  if n_elements(add) eq 0 then add = 1
  for i = start,finish-1 do begin
     ;; if mdallax[i].name ne 'LSPM_J2231+4509' and mdallax[i].name ne 'Gl768.1B' and mdallax[i].name ne 'LSPM_J0816+5704' and mdallax[i].name ne 'LSPM_J0045+0015N' and mdallax[i].name ne 'LSPM_J2049+3216W' and (mdallax[i].radius le 0.01 or (i eq start and i gt 0)) or mdallax[i].cns3 eq 'Gl 411' or mdallax[i].cns3 eq 'Gl 699' or mdallax[i].cns3 eq 'Gl 880' or mdallax[i].cns3 eq 'Gl 876' or mdallax[i].cns3 eq 'GJ 411' or mdallax[i].cns3 eq 'GJ 699' or mdallax[i].cns3 eq 'GJ 880' or mdallax[i].cns3 eq 'GJ 876' then begin 
     if mdallax[i].name ne 'poop' then begin; 'PM_I16303-1239' then begin ;if mdallax[i].uh22snr gt 150 and min((*mdallax[i].irtfspec)[*,0]) lt 0.75 then begin
        ;;  
        print,string(counter,format="(I3)")+string(9b)+string(i,format="(I3)")+string(9b)+mdallax[i].comment
        counter++
        gcal,mdallax[i],phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add
        pnts = [pnts,goodpoints]
        rchisqs = [rchisqs,rchisq_nowise]
        mdallax[i].fullspec = ptr_new(finalspec)
        mdallax[i].fulllambda = ptr_new(finallambda)
        mdallax[i].fullerr = ptr_new(finalerr)
        if rchisq_nowise gt 5 then begin
           print,'fucking rchisq>4'
           print,rchisq_nowise
           ;;stop
        endif

        ;;mdallax[i].rchisq = rchisq_nowise ;; need to add this to structure?
        ;;if rchisq_nowise gt 2 then stop
        if goodpoints lt 5 then finalfbol_err=sqrt(finalfbol_err^2.0+(0.01*finalfbol)^2.0) else finalfbol_err =sqrt(finalfbol_err^2.0+(0.005*finalfbol)^2.0)
        ;;if goodpoints lt 4 then stop
        if finalfbol_err/finalfbol lt 0.005 then finalfbol_err = 0.005*finalfbol ;; realistic floor on errors

        ;; here we calculate the important parameters!
        mdallax[i].fbol = finalfbol
        if rchisq_nowise gt 1 then finalfbol_err*=sqrt(rchisq) ;; scale up error based on chi^2
        mdallax[i].fbol_err = finalfbol_err
        fbol =  mdallax[i].fbol
        fbol_err =  mdallax[i].fbol_err
        teff =  mdallax[i].uh22model_Values[6]                                          ;; Teff from model fitting
        teff_err = sqrt(mdallax[i].uh22model_Values[7]^2.+syserr^2.)                    ;; conservative, should this be fixed at 60?
        print,mdallax[i].teff,mdallax[i].teff_err
        print,teff,teff_err
        mdallax[i].teff = teff                                                          ;; store
        mdallax[i].teff_err = teff_err                                                  ;; store
        teff = mdallax[i].teff
        tefF_err = mdallax[i].teff_Err
        theta = (fbol^.5/(teff/2341.)^2.)                                               ;; formula we always use, but reversed
        theta_err = sqrt((0.5*2341^2.*fbol^(-0.5)/(teff^2.))^2.*fbol_err^2. +$          ;;
                         (2.*2341^2.*fbol^(0.5)/teff^3.)^2.*teff_err^2.)                ;; simple propogation
        dist = 1./mdallax[i].plx                                                        ;; plx-> distance
        dist_err = dist*(mdallax[i].plx_error/mdallax[i].plx)                           ;; simple propogation
        theta_rads = (theta/1000./(60*60))*(!pi/180)                                    ;; convert to radians
        theta_Rads_err = (theta_err/1000./(60*60))*(!pi/180)                            ;; convert to radians
        coeff = (3.0856d13/695500)/2.                                                   ;; km/pc, Rsun/km, 2 for radius instead of diameter
        radius = (dist*theta_rads)*coeff                                                ;; atan(theta) ~ theta, been tested, it's fine.
        radius_err = radius*sqrt((dist_err/dist)^2.0 + (theta_rads_err/theta_rads)^2.0) ;; simple propogation
        mdallax[i].radius = radius
        mdallax[i].radius_err = radius_err


        ;; masses
        ;;jkcit = (mdallax[i].j-mdallax[i].k+0.02)/(1.068) ;; assuming CIT = K for Delfosse relation, double check later
        ;;kcit = (mdallax[i].k)-0.001*jkcit+0.019
        ;; need to move to  Johnson-Cousins-CIT system

        mk = mdallax[i].k - 5.0*(alog10(dist)-1.)
        mk+=0.019 ;; http://www.astro.caltech.edu/~jmc/2mass/v3/transformations/ 2MASS -> CIT
        mass = 10.0^(1d-3*(1.8 + 6.12*mk + 13.205*MK^2. - 6.2315*MK^3. + 0.37529*MK^4.))
        if mk lt 4.5 then mass = 10.0^(-0.1048*mk+0.3217)
        mass_err = mass*0.1
        mdallax[i].mass = mass
        mdallax[i].mass_err = mass_err

        ;;mass[where(mk gt 4.5)] += 0.1*mass[where(mk gt 4.5)]*randomn(seed)          ;;,n_elements(r[where(mk gt 4.5)])
        ;;mass[where(mk lt 4.5)] += (10^(0.065)-1.0)*mass[where(mk gt 4.5)]*randomn(seed) ;;,n_elements(r[where(mk lt 4.5)])
        ;;mass_err = 0.0*mass
        ;;mass_err[where(mk gt 4.5)] = 0.1*mass[where(mk gt 4.5)]
        ;;mass_err[where(mk lt 4.5)] = (10^(0.065)-1.0)*mass[where(mk lt 4.5)]

        print,'Teff: '+string(mdallax[i].teff,format="(I4)")
        print,'Radius: '+string(mdallax[i].radius,format="(D5.3)")
        print,'Mass: '+string(mdallax[i].mass,format="(D5.3)")
        print,'[Fe/H]: '+string(mdallax[i].irtffeh,format="(D6.3)")

        if counter mod 2 eq 1 then begin
           ;;plotter,mdallax
           ;;print,'saving'
           ;;if bin eq 0 then save,filename='~/Dropbox/Structures/mdallax.dat',mdallax else save,filename='~/Dropbox/Structures/mdallax_bin.dat',mdallax
        endif
        if stop eq 1 then begin
           stop
        endif
     endif else begin
        ;;print,mdallax[i].name
        ;;stop
     endelse
  endfor
  ;;plotter,mdallax
  ;;shrink,rchisqs
  ;;qq = where(rchisqs gt 4.0)
  ;;print,'Rchisq>4:'
  ;;if qq[0] ne -1 then print,mdallax[qq].name

  print,'not saving'
  stop
  ;; reactivate to save stuff
  ;;if bin eq 0 then save,filename='~/Dropbox/Structures/mdallax.dat',mdallax else save,filename='~/Dropbox/Structures/mdallax_bin.dat',mdallax

END

PRO plotter,mdallax,stop=stop,f=f

  ;;mdallax = mdallax[wherE(mdallax.name ne 'PM_I23318+1956W')] ;; GJ 896A, young
  if n_elements(f) eq 0 then f = 0
  ct = 3
  clip = [25,225]
  linecolor = 'blue'

  !p.font = 0
  ;; fix metallicities
  if f eq 0 then begin
     tmp = mdallax.irtffeh
     modfeh = dblarr(n_elements(mdallax))
     for i = 0,n_elements(modfeh)-1 do begin
        modfeh[i] = mean([(mdallax[i].irfharr)[1],(mdallax[i].irfharr)[2]])
     endfor
     mdallax.irtffeh = modfeh
     l = where(mdallax.comment eq 'Wide Binary') & if l[0] ne -1 then mdallax[l].irtffeh = tmp[l]
     modfeh = mdallax.irtffeh
  endif else modfeh = mdallax.feh
  sun = 'Sun'                   

  !y.margin = [4,0.5]
  !y.margin = [4,0.5]
  charsize = 1.4
  charthick = 3
  thick = 4
  if n_elements(stop) eq 0 then stop = 0
  if f eq 0 then begin
     dist = 1./mdallax.plx                                                        ;; plx-> distance
     dist_err = dist*(mdallax.plx_error/mdallax.plx)                              ;; simple propogation
     mk = mdallax.k - 5.0*(alog10(dist)-1.)
  endif else begin
     dist = mdallax.distance
     dist_err = mdallax.distance_err
     mk = mdallax.synthetic.k - 5.0*(alog10(dist)-1.)
  endelse

  ;; NEED TO ADD ERROR ON Mk
  ;; download 2MASS errors? tend to be 2%
  ;; parallax errors included
  mk_err = dblarr(n_elements(mk))
  nmonte = 250
  for i = 0,n_elements(mk)-1 do begin
     mks = dblarr(nmonte)
     for j = 0,nmonte-1 do begin
        if f eq 0 then begin
           k_tmp = mdallax[i].k + 0.02*randomn(seed) ;; for now use 0.02, but we should get the 2mass errors to do this right
        endif else begin
           k_tmp = mdallax[i].synthetic.k + mdallax[i].synthetic.k_err*randomn(seed)
        endelse
        dist_tmp = dist[i] + dist_err[i]*randomn(seed)
        mks[j] = k_tmp - 5.0*(alog10(dist_tmp)-1.)
     endfor
     mk_err[i] = robust_sigma(mks)
  endfor

  !p.font = 0
  set_plot,'PS'
  cgloadct,0
  !p.font = 0
  xmargin = !x.margin
  !x.margin = [9,9]
  device,filename='MK_R.eps',/encapsul,/color
  position = dblarr(4)
  !p.multi=[0,1,2]
  !y.margin=[1,2]
  plotsym,0,/fill
  xrange = [9.8,4.4]
  plot,mk,mdallax.radius,psym=8,xrange=xrange,yrange=[0.101,0.72],/xstyle,/ystyle,thick=4,ytitle='Radius (R!L'+sun+'!N)',/nodata,charsize=charsize,charthick=charthick,xthick=thick,ythick=thick,xtickn=strarr(10)+' '
  cgloadct,ct,clip=clip
  colors = generatearray(-0.6,0.5,100)
  vmin = 0
  vmax = 255
  colors2 = generatearray(vmin,vmax,100)
  for ii = 0,n_Elements(modfeh)-1 do oplot,[mk[ii]],[mdallax[ii].radius],psym=8,color=round(colors2[closest(modfeh[ii],colors)]),symsize=1.15,thick=4
  l = where(mdallax.radius gt 0 and mdallax.radius lt 0.8)
  if f eq 0 then metal = mdallax[l].irtffeh else metal = mdallax[l].feh
  if f eq 0 then halpha = mdallax[l].uh22ejh_ewha else halpha = mdallax[l].halpha
  name = mdallax[l].name
  x = mk[l]
  x_err = mk_err[l]
  y = mdallax[l].radius
  y_err = mdallax[l].radius_err
  cns3 = mdallax[l].cns3
  teff = mdallax[l].teff
  tmp = sort(x)
  x = x[tmp]
  x_err = x_err[tmp]
  y = y[tmp]
  y_err = y_err[tmp]
  metal = metal[tmp]
  halpha = halpha[tmp]
  name = name[tmp]
  cns3 = cns3[tmp]
  teff = teff[tmp]
  ;;;;
  yfit = 1.9515 - 0.3520*x+0.01680*x^2.0
  yfit2 = (1.9305 - 0.3466*x+0.01647*x^2.)*(1+0.04458*metal)

  ll = where(abs(yfit2-y)/y gt 0.06)
  readcol,'table2.txt',name1,name2,rah,ram,ras,decd,decm,decs,v,j,junk,junk,spt,g_feh,g_teff,format='a,a,d,d,d,d,d,d,d,d,d,a,a,d,d',delimiter=' ',/silent
  g_name = strtrim(name1,2)+'_'+strtrim(name2,2)
  g_teffcut = strarr(n_elements(ll))
  g_namecut = strarr(n_elements(ll))
  for jj = 0,n_elements(ll)-1 do begin
     q = where(name[ll[jj]] eq g_name)
     if q[0] ne -1 then begin
        g_namecut[jj] = g_name[q]
        g_teffcut[jj] = g_teff[q]
     endif
  endfor
  print,string('Name',format="(A-16)")+string(9b)+string('Match Name',format="(A-16)")+string(9b)+'CNS3    '+string(9b)+'Halpha'+string(9b)+'Teff'+string(9b)+'GaidT'+string(9b)+'[Fe/H]'+string(9b)+'Rad'+string(9b)+'Rad expected'
  forprint,string(name[ll],format="(A-16)")+string(9b)+string(g_namecut,format="(A-16)")+string(9b)+string(cns3[ll],format="(A-8)")+string(9b)+string(halpha[ll],format="(D6.3)")+string(9b)+string(teff[ll],format="(I4)")+string(9b)+string(g_teffcut,format="(I4)")+string(9b)+string(metal[ll],format="(D5.2)")+string(9b)+string(y[ll],"(D6.3)")+string(9b)+string(yfit2[ll],format="(D6.3)"),/textout

  cgloadct,0
  oplot,x,yfit,linestyle=2,thick=4,color=cgcolor(linecolor)
  position[3] = !Y.window[1]
  sharpcorners,thick=thick

  ll = where(name ne 'PM_I23318+1956W' and name ne 'PM_I23318+1956E')
  off = (y-yfit)/(y_err)
  print,'Fractional Scatter in M_K-Rad relation: '+string(robust_sigma((y[ll]-yfit[ll])/y[ll]),format="(D7.4)")
  ll = where(halpha gt 0,complement=mm)
  active = bootstrap_median(((y-yfit)/y)[ll])
  active_str = string(active[1],format="(D6.4)")+'^{+'+string(active[2]-active[1],format="(D6.4)")+'}'+'_{-'+string(active[1]-active[0],format="(D6.4)")+'}'
  inactive = bootstrap_median(((y-yfit)/y)[mm])
  inactive_str = string(inactive[1],format="(D7.4)")+'^{+'+string(inactive[2]-inactive[1],format="(D6.4)")+'}'+'_{-'+string(inactive[1]-inactive[0],format="(D6.4)")+'}'
  print,'Active star radii wrt empirical relation: '+active_str
  print,'Inactive star radii wrt empirical relation: '+inactive_str

  ;;set_plot,'x'
  ;;!p.multi=[0,1,1]
  ;;!y.margin=[5,1]
  ;;plot,halpha,(y-yfit)/y,psym=8,/xstyle,/ystyle
  ;;stop

  ;; draw an error ellipse consistent with a typical star
  ;; first find the typical star
  mkcut = 100
  radcut = 0.01
  typical = where(abs(mk-median(mk)) lt mkcut and abs(mdallax.radius_err-median(mdallax.radius_err)) lt radcut)
  typical = typical[1]
  while typical eq -1 do begin
     mkcut*=1.1
     radcut*=1.1
     typical = where(abs(mk-median(mk)) lt teffcut and abs(mdallax.radius_err-median(mdallax.radius_err)) lt radcut)
     typical = typical[0]
  endwhile
  ;; now calculate nmonte points to form the ellipse
  nmonte = 50000
  thetas = dblarr(nmonte)
  teff = mdallax[typical].teff
  teff_err = mdallax[typical].teff_err
  fbol = mdallax[typical].fbol
  fbol_err = mdallax[typical].fbol_err
  fbol = fbol+fbol_err*randomn(seed,nmonte)
  teff = teff+teff_err*randomn(seed,nmonte)
  theta = (fbol^.5/(teff/2341.)^2.)                                                     ;; formula we always use, but reversed
  if f eq 0 then begin
     k_tmp = mdallax[typical].k + 0.02*randomn(seed,nmonte) ;; for now use 0.02, but we should get the 2mass errors to do this right
  endif else begin
     k_tmp = mdallax[typical].synthetic.k + mdallax[typical].synthetic.k_err*randomn(seed,nmonte)
  endelse
  if f eq 0 then begin
     dist = 1./mdallax[typical].plx                                                        ;; plx-> distance
     dist_err = dist*(mdallax[typical].plx_error/mdallax[typical].plx)                     ;; simple propogation
  endif else begin
     dist = mdallax[typical].distance
     dist_err = mdallax[typical].distance_err
  endelse
  dist = dist+dist_err*randomn(seed,nmonte)
  mk_tmp = k_tmp - 5.0*(alog10(dist)-1.)
  theta_rads = (theta/1000./(60*60))*(!pi/180)                                   ;; convert to radians
  coeff = (3.0856d13/695500)/2.                                                  ;; km/pc, Rsun/km, 2 for radius instead of diameter
  radius = (dist*theta_rads)*coeff                                               ;; atan(theta) ~ theta, been tested, it's fine.
  mk_tmp = mk_tmp-median(mk_tmp)+8.0                                             ;; set the mk values to 0.6
  radius = radius-median(radius)+0.6                                             ;; set the radius values to ~3000
  ;; turn this into a standard deviation contour?
  bin1 = 0.01
  bin2 = 0.005
  result = hist_2d(mk_tmp,radius,bin1=bin1,bin2=bin2,min1=min(mk_tmp),max1=max(mk_tmp),min2=min(radius),max2=max(radius))
  ;; find the 67% bin
  sat = 0
  cut1 = 0
  cut2 = 0
  while sat eq 0 do begin
     cut2+=1
     cut1+=1
     frac1 = 1.0*total(result[where(result ge cut1)])/(1.0*total(result))
     frac2 = 1.0*total(result[where(result ge cut2)])/(1.0*total(result))
     if frac1 gt 0.9545 then cut1++              ;; 2 sigma
     if frac2 le 0.6827 then sat = 1 else cut2++ ;; 1 sigma
  endwhile
  xarr = generatearray(min(mk_tmp),max(mk_tmp),n_elements(result[*,0]))
  yarr = generatearray(min(radius),max(radius),n_elements(result[0,*]))
  contour,result,xarr,yarr,/overplot,levels=[cut1],/fill,C_COLOR=[cgcolor('dark grey')] ;cgcolor('grey')
  ;;
  !y.margin=[4,-1]
  cgloadct,0
  plot,x,(y-yfit)/y,psym=8,thick=4,xtitle='M!LK!N (mag)',xrange=xrange,/xstyle,/ystyle,ytitle='(Rad-Fit)/Rad',charsize=charsize,charthick=charthick,xthick=thick,ythick=thick,yrange=[-0.09,0.09]
  cgloadct,ct,clip=clip         ;
  for ii = 0,n_Elements(metal)-1 do oplot,[x[ii]],[(y[ii]-yfit[ii])/y[ii]],psym=8,color=round(colors2[closest(metal[ii],colors)]),symsize=1.15,thick=4
  cgloadct,0
  oplot,[-10,10],[0,0],thick=4,color=cgcolor(linecolor),linestyle=2
  cgloadct,ct,clip=clip
  position[[0,2]] = [!x.window[1]+0.015,!x.window[1]+0.04]
  position[1] = !Y.window[0]
  cgcolorbar,/vertical,ticknames=['-0.4','-0.2','+0.0','+0.2','+0.4'],divisions=4,ytickv=round([interpol(colors2,colors,-0.4),interpol(colors2,colors,-0.2),interpol(colors2,colors,0.0),interpol(colors2,colors,0.2),interpol(colors2,colors,0.4)]),/right,position=position,title='[Fe/H]',ticklen=-0.25,charsize=0.0,AnnotateColor=cgcolor('black',-100)
  sharpcorners,thick=thick
  device,/close

  ;; stats:
  print,'M_K-[Fe/H] correlation: ',r_correlate(metal,(y-yfit)/y)
  ;;print,'Stdev residual: ',robust_sigma((y-yfit)/y)
  rchisq=total((y-yfit)^2.0/(y_err^2.0))/n_elements(y)
  print,'Reduced Chi^2: ',rchisq

  slopes = dblarr(nmonte)
  for nn = 0d0,nmonte-1d0 do begin
     metal_tmp = metal+0.08*randomn(seed,n_elements(metal))
     y_tmp = y+y_err*randomn(seed,n_elements(y))
     res_tmp = linfit(metal_tmp,(y_tmp-yfit)/y_tmp,measure_errors=0.08+dblarr(n_elements(metal)))
     slopes[nn] = res_tmp[1]
  endfor
  res = linfit(metal,(y-yfit)/y,measure_errors=0.08+dblarr(n_elements(metal)),sigma=sigma)
  ;;print,'Slope on MK vs [Fe/H]= '+string(res[1],format="(D6.3)")
  slp_tmp = slopes[sort(slopes)]
  print,'Max deviation per 1 dex of metallicity (MK-[Fe/H]) = '+string(slp_tmp[nmonte*0.997],format="(D8.4)")+','+string(slp_tmp[nmonte*0.003],format="(D8.4)")



 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; device,filename='MJ_L.eps',/encapsul,/color
  ;; loglum = alog10(mdallax.luminosity)
  ;; loglum_err = alog10(2.71828)*(mdallax.luminosity_err/mdallax.luminosity)
  ;; position = dblarr(4)
  ;; !p.multi=[0,1,2]
  ;; !y.margin=[1,2]
  ;; plotsym,0,/fill
  ;; xrange = [9.8,4.4]
  ;; plot,mk,loglum,psym=8,xrange=xrange,/xstyle,/ystyle,thick=4,ytitle='log!L10!N(L!LSun!N)',/nodata,charsize=charsize,charthick=charthick,xthick=thick,ythick=thick,xtickn=strarr(10)+' '
  ;; cgloadct,ct,clip=clip
  ;; colors = generatearray(-0.6,0.5,100)
  ;; vmin = 0
  ;; vmax = 255
  ;; colors2 = generatearray(vmin,vmax,100)
  ;; for ii = 0,n_Elements(modfeh)-1 do oplot,[mk[ii]],[loglum[ii]],psym=8,color=round(colors2[closest(modfeh[ii],colors)]),symsize=1.15,thick=4
  ;; l = where(mdallax.radius gt 0 and mdallax.radius lt 0.8)
  ;; if f eq 0 then metal = mdallax[l].irtffeh else metal = mdallax[l].feh
  ;; if f eq 0 then halpha = mdallax[l].uh22ejh_ewha else halpha = mdallax[l].halpha
  ;; name = mdallax[l].name
  ;; x = mdallax.synthetic.j-5.0*(alog10(mdallax.distance)-1.)
  ;; x_err = mk_err[l]
  ;; y = loglum[l]
  ;; y_err = loglum_err[l]
  ;; tmp = sort(x)
  ;; x = x[tmp]
  ;; x_err = x_err[tmp]
  ;; y = y[tmp]
  ;; y_err = y_err[tmp]
  ;; metal = metal[tmp]
  ;; halpha = halpha[tmp]
  ;; name = name[tmp]
  ;; ;;;;
  ;; start_params = dblarr(5)
  ;; fit = MPFITFUN('mypoly', X, Y, sqrt((y_err/y)^2. + (x_err/x)^2.), start_params,/quiet)
  ;; yfit = mypoly(x,fit)

  ;; cgloadct,0
  ;; oplot,x,yfit,linestyle=2,thick=4,color=cgcolor(linecolor)
  ;; position[3] = !Y.window[1]
  ;; sharpcorners,thick=thick

  ;; ll = where(name ne 'PM_I23318+1956W' and name ne 'PM_I23318+1956E')
  ;; off = (y-yfit)/(y_err)
  ;; print,'Scatter in M_K-log(Lum) relation: '+string(robust_sigma((y[ll]-yfit[ll])),format="(D7.4)")
  ;; ll = where(halpha gt 0,complement=mm)
  ;; active = bootstrap_median(((y-yfit))[ll])
  ;; active_str = string(active[1],format="(D6.4)")+'^{+'+string(active[2]-active[1],format="(D6.4)")+'}'+'_{-'+string(active[1]-active[0],format="(D6.4)")+'}'
  ;; inactive = bootstrap_median(((y-yfit)/y)[mm])
  ;; inactive_str = string(inactive[1],format="(D7.4)")+'^{+'+string(inactive[2]-inactive[1],format="(D6.4)")+'}'+'_{-'+string(inactive[1]-inactive[0],format="(D6.4)")+'}'
  ;; print,active_str
  ;; print,inactive_str

  ;; ;; draw an error ellipse consistent with a typical star
  ;; ;; first find the typical star
  ;; mkcut = 100
  ;; radcut = 0.01
  ;; typical = where(abs(mk-median(mk)) lt mkcut and abs(mdallax.luminosity_err-median(mdallax.luminosity_err)) lt radcut)
  ;; typical = typical[1]
  ;; while typical eq -1 do begin
  ;;    mkcut*=1.1
  ;;    radcut*=1.1
  ;;    typical = where(abs(mk-median(mk)) lt teffcut and abs(mdallax.luminosity_err-median(mdallax.luminosity_err)) lt radcut)
  ;;    typical = typical[0]
  ;; endwhile
  ;; ;; now calculate nmonte points to form the ellipse
  ;; nmonte = 50000
  ;; thetas = dblarr(nmonte)
  ;; teff = mdallax[typical].teff
  ;; teff_err = mdallax[typical].teff_err
  ;; fbol = mdallax[typical].fbol
  ;; fbol_err = mdallax[typical].fbol_err
  ;; fbol = fbol+fbol_err*randomn(seed,nmonte)
  ;; teff = teff+teff_err*randomn(seed,nmonte)
  ;; theta = (fbol^.5/(teff/2341.)^2.)                                                     ;; formula we always use, but reversed
  ;; if f eq 0 then begin
  ;;    k_tmp = mdallax[typical].k + 0.02*randomn(seed,nmonte) ;; for now use 0.02, but we should get the 2mass errors to do this right
  ;; endif else begin
  ;;    k_tmp = mdallax[typical].synthetic.k + mdallax[typical].synthetic.k_err*randomn(seed,nmonte)
  ;; endelse
  ;; if f eq 0 then begin
  ;;    dist = 1./mdallax[typical].plx                                                        ;; plx-> distance
  ;;    dist_err = dist*(mdallax[typical].plx_error/mdallax[typical].plx)                     ;; simple propogation
  ;; endif else begin
  ;;    dist = mdallax[typical].distance
  ;;    dist_err = mdallax[typical].distance_err
  ;; endelse
  ;; dist = dist+dist_err*randomn(seed,nmonte)
  ;; mk_tmp = k_tmp - 5.0*(alog10(dist)-1.)
  ;; c1 = (4.*!pi * 1d4) / 3.865d33                                                 ;;3.939d33
  ;; c2 = 3.08567758d18
  ;; c = c1*c2^2.0
  ;; luminosity = c*(fbol*1d-12*(dist)^2.0)

  ;; ;; turn this into a standard deviation contour?
  ;; bin1 = 0.01
  ;; bin2 = 0.005
  ;; result = hist_2d(mk_tmp,luminosity,bin1=bin1,bin2=bin2,min1=min(mk_tmp),max1=max(mk_tmp),min2=min(luminosity),max2=max(luminosity))
  ;; ;; find the 67% bin
  ;; sat = 0
  ;; cut1 = 0
  ;; cut2 = 0
  ;; while sat eq 0 do begin
  ;;    cut2+=1
  ;;    cut1+=1
  ;;    frac1 = 1.0*total(result[where(result ge cut1)])/(1.0*total(result))
  ;;    frac2 = 1.0*total(result[where(result ge cut2)])/(1.0*total(result))
  ;;    if frac1 gt 0.9545 then cut1++              ;; 2 sigma
  ;;    if frac2 le 0.6827 then sat = 1 else cut2++ ;; 1 sigma
  ;; endwhile
  ;; xarr = generatearray(min(mk_tmp),max(mk_tmp),n_elements(result[*,0]))
  ;; yarr = generatearray(min(luminosity),max(luminosity),n_elements(result[0,*]))
  ;; contour,result,xarr,yarr,/overplot,levels=[cut1],/fill,C_COLOR=[cgcolor('dark grey')] ;cgcolor('grey')
  ;; ;;
  ;; !y.margin=[4,-1]
  ;; cgloadct,0
  ;; plot,x,(y-yfit),psym=8,thick=4,xtitle='M!LK!N (mag)',xrange=xrange,/xstyle,/ystyle,ytitle='log!L10!N(L!LSun!N)-Fit',charsize=charsize,charthick=charthick,xthick=thick,ythick=thick
  ;; cgloadct,ct,clip=clip         ;
  ;; for ii = 0,n_Elements(metal)-1 do oplot,[x[ii]],[(y[ii]-yfit[ii])],psym=8,color=round(colors2[closest(metal[ii],colors)]),symsize=1.15,thick=4
  ;; cgloadct,0
  ;; oplot,[-10,10],[0,0],thick=4,color=cgcolor(linecolor),linestyle=2
  ;; cgloadct,ct,clip=clip
  ;; position[[0,2]] = [!x.window[1]+0.015,!x.window[1]+0.04]
  ;; position[1] = !Y.window[0]
  ;; cgcolorbar,/vertical,ticknames=['-0.4','-0.2','+0.0','+0.2','+0.4'],divisions=4,ytickv=round([interpol(colors2,colors,-0.4),interpol(colors2,colors,-0.2),interpol(colors2,colors,0.0),interpol(colors2,colors,0.2),interpol(colors2,colors,0.4)]),/right,position=position,title='[Fe/H]',ticklen=-0.25,charsize=0.0,AnnotateColor=cgcolor('black',-100)
  ;; sharpcorners,thick=thick
  ;; device,/close
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  !y.margin = [1,2]
  xrange = [2700,4199]
  device,filename='T_R.eps',/encapsul,/color
  cgloadct,0
  plot,mdallax.teff,mdallax.radius,psym=8,xrange=xrange,yrange=[0.101,0.72],/xstyle,/ystyle,thick=4,ytitle='Radius (R!L'+sun+'!N)',charsize=charsize,charthick=charthick,xthick=thick,ythick=thick,xtickn=' '+strarr(10) ;mdallax.teff_err,mdallax.radius_err,errthick=4,xtitle='T!Leff!N (K)'
  if f eq 0 then modfeh = mdallax.irtffeh else modfeh = mdallax.feh
  ;; circle h-alpha points
  ;;if f eq 1 then halpha = mdallax.halpha else halpha = mdallax.uh22ejh_ewha
  ;;ll = where(halpha+0.37 gt 1.5)
  ;;plotsym,0
  ;;if ll[0] ne -1 then oplot,mdallax[ll].teff,mdallax[ll].radius,psym=8,symsize=1.75
  ;;plotsym,0,/fill
  cgloadct,ct,clip=clip
  colors = generatearray(-0.6,0.5,100)
  vmin = 0
  vmax = 255
  colors2 = generatearray(vmin,vmax,100)
  for ii = 0,n_Elements(modfeh)-1 do begin
     oplot,[mdallax[ii].teff],[mdallax[ii].radius],psym=8,color=round(colors2[closest(modfeh[ii],colors)]),symsize=1.15,thick=4 ;,[mdallax[ii].teff_err],[mdallax[ii].radius_err],errthick=4,errcolor=round(colors2[closest(modfeh[ii],colors)])
  endfor
  ;;teff = generatearray(3220,4773,100)
  ;;rad = -22.296508+ 0.015446387*teff - 3.4884521d-06*(teff^2.0)+(2.6496096d-10)*(teff^3.0)
  ;;oplot,teff,rad,thick=3,color=cgcolor(linecolor)
  cgloadct,0

  position = dblarr(4)
  position[3] = !Y.window[1]

  ;; draw an error ellipse consistent with a typical star
  ;; first find the typical star
  teffcut = 100
  radcut = 0.001
  typical = where(abs(mdallax.teff-median(mdallax.teff)) lt teffcut and abs(mdallax.radius_err-median(mdallax.radius_err)) lt radcut)
  if n_elements(typical) gt 7 then mark = 7 else mark = n_Elements(typical)-1
  typical = typical[mark]
  print,mdallax[typical].teff
  while typical eq -1 do begin
     teffcut*=1.1
     radcut*=1.1
     typical = where(abs(mdallax.teff-median(mdallax.teff)) lt teffcut and abs(mdallax.radius_err-median(mdallax.radius_err)) lt radcut)
     typical = typical[0]
  endwhile
  ;; now calculate nmonte points to form the ellipse
  nmonte = 50000
  thetas = dblarr(nmonte)
  teff = mdallax[typical].teff
  teff_err = mdallax[typical].teff_err
  fbol = mdallax[typical].fbol
  fbol_err = mdallax[typical].fbol_err
  fbol = fbol+fbol_err*randomn(seed,nmonte)
  teff = teff+teff_err*randomn(seed,nmonte)
  theta = (fbol^.5/(teff/2341.)^2.)                                                     ;; formula we always use, but reversed
  if f eq 0 then begin
     dist = 1./mdallax[typical].plx                                                        ;; plx-> distance
     dist_err = dist*(mdallax[typical].plx_error/mdallax[typical].plx)                     ;; simple propogation
  endif else begin
     dist = mdallax[typical].distance
     dist_err = mdallax[typical].distance_err
  endelse
  dist = dist+dist_err*randomn(seed,nmonte)
  theta_rads = (theta/1000./(60*60))*(!pi/180)                                   ;; convert to radians
  coeff = (3.0856d13/695500)/2.                                                  ;; km/pc, Rsun/km, 2 for radius instead of diameter
  radius = (dist*theta_rads)*coeff                                               ;; atan(theta) ~ theta, been tested, it's fine.
  teff = teff-median(teff)+3000                                                  ;; set the teff values to ~3000
  radius = radius-median(radius)+0.6                                             ;; set the radius values to ~3000
  ;; turn this into a standard deviation contour?
  bin1 = 5
  bin2 = 0.005
  result = hist_2d(teff,radius,bin1=10,bin2=0.01,min1=min(teff),max1=max(teff),min2=min(radius),max2=max(radius))
  ;; find the 67% bin
  sat = 0
  cut1 = 0
  cut2 = 0
  while sat eq 0 do begin
     cut2+=1
     cut1+=1
     frac1 = 1.0*total(result[where(result ge cut1)])/(1.0*total(result))
     frac2 = 1.0*total(result[where(result ge cut2)])/(1.0*total(result))
     if frac1 gt 0.9545 then cut1++              ;; 2 sigma
     if frac2 le 0.6827 then sat = 1 else cut2++ ;; 1 sigma
  endwhile
  xarr = generatearray(min(teff),max(teff),n_elements(result[*,0]))
  yarr = generatearray(min(radius),max(radius),n_elements(result[0,*]))
  contour,result,xarr,yarr,/overplot,levels=[cut1],/fill,C_COLOR=[cgcolor('dark grey')] ;[cgcolor('grey')


  l = where(mdallax.radius gt 0 and mdallax.radius lt 0.8)
  name = mdallax[l].name
  cns3 = mdallax[l].cns3
  if f eq 0 then halpha = mdallax[l].uh22ejh_ewha else halpha = mdallax[l].halpha
  if f eq 0 then metal = mdallax[l].irtffeh else metal = mdallax[l].feh
  if f eq 0 then metal_err = 0.08+dblarr(n_elements(l)) else metal_err = mdallax[l].feh_err
  x = mdallax[l].teff
  x_err = mdallax[l].teff_err
  y = mdallax[l].radius
  y_err = mdallax[l].radius_err
  tmp = sort(x)
  x = x[tmp]
  x_err = x_err[tmp]
  y = y[tmp]
  y_err = y_err[tmp]
  halpha = halpha[tmp]
  name = name[tmp]
  metal = metal[tmp]
  metal_err = metal_err[tmp]
  cns3 = cns3[tmp]
  fit = polyfit(x,y,4,yfit)
  ;;yfit = +5.43d-4*(x-3500)-6.77d-8*(x-3500)^2.-4.86d-10*(x-3500)^3.;+0.1576*metal
  yfit = 10.5440 - 33.7546*(x/3500.) + 35.1909*(x/3500.)^2. - 11.5928*(x/3500.)^3.
  cgloadct,0
  oplot,x,yfit,linestyle=2,thick=4,color=cgcolor(linecolor)
  ;;stop
  ;;ploterror,x,(y-yfit)/y,x_err,y_err/y,psym=8,errthick=4,thick=4,xtitle='T!LEFF!N',xrange=[2850,4199],ytitle='(Rad-Fit)/Rad',charsize=charsize,charthick=charthick
  !y.margin = [4,-1]
  plot,x,(y-yfit)/y,psym=8,xtitle='T!Leff!N (K)',xrange=xrange,/xstyle,ytitle='(Rad-Fit)/Rad',charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,/ystyle
  cgloadct,ct,clip=clip         ;[75,225]
  for ii = 0,n_Elements(metal)-1 do oplot,[x[ii]],[(y[ii]-yfit[ii])/y[ii]],psym=8,color=round(colors2[closest(metal[ii],colors)]),symsize=1.15,thick=4 ;;,[x_err[ii]],[y_err[ii]/y[ii]],errcolor=[round(colors2[closest(metal[ii],colors)])round(colors2[closest(metal[ii],colors)])],errthick=4,

  ;;plotsym,0,thick=3
  ;;l = where(halpha gt 1)
  ;;oplot,x[l],((y-yfit)/y)[l],symsize=2.5,psym=8
  ;;plotsym,0,/fill

  cgloadct,0
  oplot,[0,1d5],[0,0],thick=4,color=cgcolor(linecolor),linestyle=2
  sharpcorners,thick=thick
  position[[0,2]] = [!x.window[1]+0.015,!x.window[1]+0.04]
  position[1] = !y.window[0]
  
  cgloadct,ct,clip=clip         ;[75,225]
  cgcolorbar,/vertical,ticknames=['-0.4','-0.2','+0.0','+0.2','+0.4'],divisions=4,ytickv=round([interpol(colors2,colors,-0.4),interpol(colors2,colors,-0.2),interpol(colors2,colors,0.0),interpol(colors2,colors,0.2),interpol(colors2,colors,0.4)]),/right,position=position,title='[Fe/H]',ticklen=-0.25,charsize=0.0,AnnotateColor=cgcolor('black',-100)
  device,/close

  rad_test = (16.7700-54.3210*(x/3500.)+57.6627*(x/3500.)^2.-19.6994*(x/3500.)^3.)*(1+0.4565*metal)
  ;;params = [0.41163461, 0.00051821639, -2.0384777e-07, -3.7072691e-10, 3.9202823e-13, 0.16132138]

  ;start_params = [0,0,0,0,0,0]
  ;params = MPFIT2DFUN('TR_FIT', x, metal, y, y_err, start_params)
  ;mass_test = tr_fit(x,metal,params)
  
  ;;let's do a full MCMC on this thing:
  ;;nmonte = 10000
  ;;y_errs = dblarr(n_elements(x)) ;; this is the error in the resulting y we get due to errors in teff, metallicity
  ;;for kk = 0,n_elements(x)-1 do begin
  ;;   teff_tmp = x[kk]+x_err[kk]*randomn(seed,nmonte)
  ;;   metal_tmp = metal[kk]+metal_err[kk]*randomn(seed,nmonte)
  ;;   rad_tmp = tr_fit(teff_tmp,metal_tmp,params)
  ;;   y_errs[kk] = robust_sigma(rad_tmp)
  ;;endfor
  y_errs = rad_test*0.09
  
  gg = where(mdallax.cns3 ne 'GJ 896A' and mdallax.cns3 ne 'GJ 896B')
  print,'Scatter of T-R calibration (fractional) = '+string(stdev((y[gg]-rad_test[gg])/y[gg]),format="(D7.4)")
  gg = wherE(mdallax.teff gt 3500)
  print,'Scatter of T-R calibration (fractional) just >3500K = '+string(stdev((y[gg]-rad_test[gg])/y[gg]),format="(D7.4)")
  gg = wherE(mdallax.teff lt 3500)
  print,'Scatter of T-R calibration (fractional) just <3500K = '+string(stdev((y[gg]-rad_test[gg])/y[gg]),format="(D7.4)")
  set_plot,'x'
  !p.multi=[0,1,1]
  !y.margin=[4,1]
  plot,rad_test,y,psym=8,/xstyle,/ystyle
  ll = where(abs((rad_test-y)/y_errs) gt 2.0) ; or name eq 'PM_I23318+1956W')
  
  gg = where(halpha lt 6000)
  rchiqs = total((rad_test[gg]-y[gg])^2./y_errs[gg]^2.)/n_elements(rad_test[gg])
  print,'Rchisq = '+string(rchiqs,format="(D5.2)")
  if ll[0] ne -1 then begin
     readcol,'table2.txt',name1,name2,rah,ram,ras,decd,decm,decs,v,j,junk,junk,spt,g_feh,g_teff,format='a,a,d,d,d,d,d,d,d,d,d,a,a,d,d',delimiter=' ',/silent
     g_name = strtrim(name1,2)+'_'+strtrim(name2,2)
     g_teffcut = strarr(n_elements(ll))
     g_namecut = strarr(n_elements(ll))
     for jj = 0,n_elements(ll)-1 do begin
        q = where(name[ll[jj]] eq g_name)
        if q[0] ne -1 then begin
           g_namecut[jj] = g_name[q]
           g_teffcut[jj] = g_teff[q]
        endif
     endfor
     print,string('Name',format="(A-16)")+string(9b)+string('Match Name',format="(A-16)")+string(9b)+'CNS3    '+string(9b)+'Halpha'+string(9b)+'Teff'+string(9b)+'GaidT'+string(9b)+'[Fe/H]'+string(9b)+'Rad'+string(9b)+'Rad expected'
     forprint,string(name[ll],format="(A-16)")+string(9b)+string(g_namecut,format="(A-16)")+string(9b)+string(cns3[ll],format="(A-8)")+string(9b)+string(halpha[ll],format="(D6.3)")+string(9b)+string(x[ll],format="(I4)")+string(9b)+string(g_teffcut,format="(I4)")+string(9b)+string(metal[ll],format="(D5.2)")+string(9b)+string(y[ll],"(D6.3)")+string(9b)+string(rad_test[ll],format="(D6.3)"),/textout
  endif
  set_plot,'PS'

  !y.margin=[4,2]
  !p.multi=[0,1,1]
  device,filename='metal_rad.eps',/encapsul,/color
  plot,metal,(y-yfit)/y,psym=8,xtitle='[Fe/H]',/xstyle,/ystyle,ytitle='(Radius - Fit)/Radius',thick=4,charsize=charsize,charthick=charthick,xthick=thick,ythick=thick
  oploterror,[0.4],[-0.3],[0.08],[median(y_err)],psym=3,errthick=3

  cgloadct,1,clip=[75,225]
  colors = generatearray(2800,4200,100)
  vmin = 0
  vmax = 255
  colors2 = generatearray(vmin,vmax,100)
  for ii = 0,n_Elements(metal)-1 do oplot,[metal[ii]],[(y[ii]-yfit[ii])/y[ii]],psym=8,color=(round(colors2[closest(x[ii],colors)]))[0],symsize=1.15,thick=4 ;;,,errcolor=[round(colors2[closest(metal[ii],colors)])round(colors2[closest(metal[ii],colors)])],errthick=4,
  cgcolorbar,/vertical,ticknames=['2900','3200','3500','3800','4100'],divisions=4,ytickv=round([interpol(colors2,colors,2900),interpol(colors2,colors,3200),interpol(colors2,colors,3500),interpol(colors2,colors,3800),interpol(colors2,colors,4100)]),/right,position=position,title='T!Leff!N',ticklen=-0.25,charsize=0.0,AnnotateColor=cgcolor('black',-100)
  res = linfit(metal,(y-yfit)/y,measure_errors=0.08+dblarr(n_elements(metal)),sigma=sigma)
  device,/close

  print,'T_R - [Fe/H] correlation: ',r_correlate(metal,(y-yfit)/y)

  nmonte = 1000
  slopes = dblarr(nmonte)
  for nn = 0,nmonte-1 do begin
     metal_tmp = metal+0.08*randomn(seed,n_elements(metal))
     y_tmp = y+y_err*randomn(seed,n_elements(y))
     res_tmp = linfit(metal_tmp,(y_tmp-yfit)/y_tmp,measure_errors=0.08+dblarr(n_elements(metal)))
     slopes[nn] = res_tmp[1]
  endfor
  res = linfit(metal,(y-yfit)/y,measure_errors=0.08+dblarr(n_elements(metal)),sigma=sigma)
  print,'Slope on T-R relation= '+string(res[1],format="(D6.3)")
  slp_tmp = slopes[sort(slopes)]
  print,'Max deviation per 1 dex of metallicity (T-[Fe/H]) = '+string(slp_tmp[nmonte*0.997],format="(D8.4)")+','+string(slp_tmp[nmonte*0.003],format="(D8.4)")

  ;; potential baddies:
  above = where((y-yfit)/y_err gt 1. and metal lt -0.1)
  below = where((y-yfit)/y_err lt -1. and metal gt 0.0)

  ;; if above[0] ne -1 then begin
  ;;    print,'Big but metal-poor'
  
  ;;    forprint,name[above]+string(9b)+strtrim(strmid(name[above],4,15),2)+String(9b)+string(cns3[above],format="(A8)"),((y-yfit)/y)[above],x[above],y[above],metal[above],/textout
  ;; endif
  ;; if below[0] ne -1 then begin
  ;;    print,'Small but rich'
  ;;    forprint,name[below]+string(9b)+string(cns3[below],format="(A8)"),((y-yfit)/y)[below],x[below],y[below],metal[below],/textout
  ;; endif

  set_plot,'x'
  !x.margin = xmargin
  if stop eq 1 then stop

END

function mypoly,x,p
  y = 0
  for i = 0,n_elements(p)-1 do y+=p[i]*x^(i*1.0)
  return,y
end

function TR_fit,teff,metal,p
  rad = 0
  for i = 0,n_elements(p)-2 do rad+= p[i]*teff^(1.*i)
  rad+=p[i]*metal
  return,rad
end


PRO gcal_wrapper,name = name,redo=redo,add=add

  set_plot,'x'
  if n_elements(redo) eq 0 then redo = 0
  if n_elements(hot) eq 0 then hot = 0
  device,retain=2
  restore,'~/Dropbox/structures/rdtargets.dat'
  restore,'~/Dropbox/structures/targets6.dat'
  t = targets[where(Targets.uh22snr gt 100 and targets.irtfsnr gt 100 and targets.uh22numspectype gt 5.0)]
  tmp = rdtargets.cns3
  strreplace,tmp,'GJ','Gliese'
  strreplace,tmp,' ',''
  strreplace,tmp,'Gliese','Gliese\ '
  rdtargets.othername = tmp

  
  l = where(rdtargets.cns3 eq 'GJ 876') & if l[0] ne -1 then rdtargets[l].othername = 'HIP\ 113020'
  l = where(rdtargets.cns3 eq 'GJ 176') & if l[0] ne -1 then rdtargets[l].othername = 'HD285968'
  l = where(rdtargets.cns3 eq 'GJ 649') & if l[0] ne -1 then rdtargets[where(rdtargets.cns3 eq 'GJ 649')].othername = 'Gliese\ 649'
  l = where(rdtargets.cns3 eq 'GJ752A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ752A')].othername = 'HD\ 180617'
  l = where(rdtargets.cns3 eq 'GJ 15A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 15A')].othername = 'HD\ 001326'
  l = where(rdtargets.cns3 eq 'GJ 699') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 699')].othername = '0980140024'
  l = where(rdtargets.cns3 eq 'GJ 338A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 338A')].othername = '0100079210'
  l = where(rdtargets.cns3 eq 'GJ 412A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 412A')].othername = '0004402051'
  l = where(rdtargets.cns3 eq 'GJ 570A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 570A')].othername = 'HD\ 131977'
  l = where(rdtargets.cns3 eq 'GJ 820A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 820A')].othername = 'HD\ 201091'
  l = where(rdtargets.cns3 eq 'GJ 820B') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 820B')].othername = 'HD\ 201092'
  l = where(rdtargets.cns3 eq 'GJ 105A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 105A')].othername = 'HD\ 016160'
  l = where(rdtargets.cns3 eq 'GJ 702B') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 702B')].othername = 'HD\ 165341'
  l = where(rdtargets.cns3 eq 'GJ 725A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 725A')].othername = 'HD\ 173739'
  l = where(rdtargets.cns3 eq 'GJ 725B') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 725B')].othername = 'HD\ 173739\ B'
  l = where(rdtargets.cns3 eq 'GJ 896A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 896A')].othername = 'Gliese\ 896'
  l = where(rdtargets.cns3 eq 'GJ 208') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 245409'
  l = where(rdtargets.name eq 'HD_209458') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 209458'
  l = where(rdtargets.name eq 'HD_189733') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 189733'


  ;; added Jan 2015
  l = where(rdtargets.name eq 'GJ166A') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 26965'
  l = where(rdtargets.name eq 'GJ631') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 149661'
  l = where(rdtargets.name eq 'GJ_706') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 166620'
  l = where(rdtargets.name eq 'GJ_105A') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 16160'
  l = where(rdtargets.name eq 'PM_I09144+5241') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 79211'
  l = where(rdtargets.name eq 'PM_I14513+1906') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 131156'

  ;;ra = rdtargets[where(rdtargets.cns3 eq 'GJ 551')].ra
  ;;dec = rdtargets[where(rdtargets.cns3 eq 'GJ 551')].dec
  ;;nm = rdtargets[where(rdtargets.cns3 eq 'GJ 551')].name
  ;;cns3 = rdtargets[where(rdtargets.cns3 eq 'GJ 551')].cns3
  ;;othername = rdtargets[where(rdtargets.cns3 eq 'GJ 551')].othername
  ;;r1 = rdtargets[where(rdtargets.cns3 eq 'GJ 551')]
  ;;r2 = targets[where(targets.name eq 'PM_I11055+4331')];rdtargets[where(rdtargets.cns3 eq 'GJ 905')]; ;;PM_I02530+1652, 
  ;;struct_assign,r2,r1
  ;;r1.ra = ra
  ;;r1.dec = dec
  ;;r1.cns3 = cns3
  ;;r1.name = nm
  ;;r1.othername = othername
  ;;rdtargets[where(rdtargets.cns3 eq 'GJ 551')] = r1

  ;;qwer = where((Rdtargets.irtfsnr gt 60 or rdtargets.irsnr gt 60) and rdtargets.uh22snr gt 60 and strmid(rdtargets.comment,0,3) eq 'Tab' and rdtargets.newfbol gt 0)
  ;;qwer = where(strpos(Rdtargets.comment,'Tab_2015A') ne -1 and rdtargets.uh22snr gt 80 and rdtargets.irtfsnr gt 80)
  ;;print,n_elements(qwer)
  ;;print,qwer
  qwer = where(Rdtargets.irsnr gt 60 and rdtargets.uh22snr gt 60 and rdtargets.comment eq 'Tabetha')
  if n_elements(name) eq 1 then qwer = where(rdtargets.cns3 eq name)
  if qwer[0] eq -1 then qwer = where(rdtargets.name eq name)
  if qwer[0] eq -1 then stop
  h = rdtargets[qwer]

  oldfbol = dblarr(n_elements(h))
  fancyfbol = dblarr(n_elements(h))
  fancyfbol_err = fancyfbol
  oldmyfbol = dblarr(n_elements(h))
  rchisqs = dblarr(n_elements(h))
  restore,'~/Dropbox/Radii/phot_systems.dat'
  ;;restore,'~/Dropbox/Radii/phot_systems_base.dat'
  ;;restore,'~/Dropbox/Radii/phot_systems_mod.dat'
  print,n_elements(qwer)
  if n_elements(name) eq '' then name = ' '
  for i = 0,n_Elements(h)-1 do begin
     if h[i].cns3 eq name or name eq ' ' or h[i].name eq name then begin
        ;;if strpos(h[i].uh22spectype,'M') ne -1 then add = 1 else add = 0
        gcal,h[i],phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,add=add,long=-1
        oldfbol[i] = h[i].fbol
        oldmyfbol[i] = h[i].newfbol
        fancyfbol[i] = finalfbol
        fancyfbol_err[i] = finalfbol_err
        rchisqs[i] = rchisq
        l = where(rdtargets.name eq h[i].name)
        if l[0] eq -1 then begin
           print,'PROBLEM'
           stop
        endif
        rdtargets[l].fullspec = ptr_new(finalspec)
        rdtargets[l].fulllambda = ptr_new(finallambda)
        rdtargets[l].fullerr = ptr_new(finalerr)
        rdtargets[l].newfbol = finalfbol
        rdtargets[l].newfbol_err = finalfbol_err
     endif
  endfor
  forprint,h.cns3+string(9b),oldfbol,oldmyfbol*1d12,h.fbol_err,fancyfbol,h.teff,h.newteff,h.teff*(fancyfbol/h.fbol)^(1/4.),/textout
  print,'Program Complete'
  forprint,h.cns3+string(9b),fancyfbol,fancyfbol_err,rchisqs,/textout
  stop
  save,filename='~/Dropbox/Structures/rdtargets.dat',rdtargets

END


PRO gcal,r,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add,long=long,inset=inset,oc=oc,hot=hot,printname=printname

  set_plot,'x'

  if n_elements(redo) eq 0 then redo = 0
  if n_elements(add) eq 0 then add = 1
  if n_elementS(hot) eq 0 then hot = 0
  device,retain=2
  charthick = 3
  charsize = 1.25
  xthick = 5
  ythick = 5
  thick = 3.0
  errthick = 3.0
  plotsym,0,/fill
  xrange = [0.3,4.0]            ;[0.25,0.6];
  xrange_blue = [0.25,0.6]
  xrange_red = [2.0,30]
  !except = 0
  !p.font = 0
  set_plot,'x'
  symsize=1.1

  !p.multi=[0,1,1]
  name = r.cns3
  strreplace,name,' ',''
  name = r.cns3
  strreplace,name,' ',''
  strreplace,name,' ',''
  strreplace,name,' ',''
  strreplace,name,' ',''
  strreplace,name,' ',''
  strreplace,name,' ',''
  if strtrim(name,2) eq '' then name = r.name
  rem = ''
  if r.cns3 eq 'GJ752A' then rem = 'AV'
  if r.cns3 eq 'GJ 15A' then rem = 'ABV'
  ;;if r.cns3 eq 'GJ 338A' then rem = 'A'
  ;;if r.cns3 eq 'GJ 338B' then rem = 'B'
  if r.cns3 eq 'GJ 412A' then rem = 'A'
  if r.cns3 eq 'GJ 570A' then rem = 'A'
  if r.cns3 eq 'GJ 809' then rem = 'A'
  if r.cns3 eq 'GJ 820A' then rem = 'AV'
  if r.cns3 eq 'GJ 105A' then rem = 'AP'
  if r.cns3 eq 'GJ 702B' then rem = 'B'
  if r.cns3 eq 'GJ 896A' then rem = 'ABV'
  if r.name eq 'GJ 896B' then rem = 'B'
  if r.name eq 'PM_I00184+4401' then rem = 'BV'
  if r.name eq 'PM_I02555+2652' then rem = 'C'
  if r.name eq 'PM_I02556+2652S' then rem = 'B'
  if r.name eq 'PM_I06171+0507' then rem = 'B'
  if r.name eq 'PM_I08526+2818' then rem = 'B'
  if r.name eq 'PM_I11054+4331' then rem = 'A'
  if r.name eq 'PM_I13062+2043' then rem = 'A'
  if r.name eq 'PM_I13283-0221Ww' then rem = 'A'
  if r.name eq 'PM_I18427+5937N' then rem = 'A'
  if r.name eq 'PM_I18427+5937S' then rem = 'B'
  if r.name eq 'PM_I02555+2652' then rem = 'C'
  if r.name eq 'PM_I22387-2037' then rem = 'AV'
  if r.name eq 'PM_I23318+1956W' then rem = 'AV'
  if r.name eq 'PM_I05024-2115' then rem = 'AB'
  if r.name eq 'PM_I16554-0820' then rem = 'AB'
  if r.name eq 'PM_I19169+0510' then rem = 'AV'
  if r.name eq 'PM_I07232+4605' then rem = 'A'
  if r.cns3 eq 'GJ 725A' then rem = 'A'
  if r.cns3 eq 'GJ 725B' then rem = ''
  if r.cns3 eq 'GJ 81.1' or r.name eq 'PM_I01571-1014E' then rem = 'B'
  if r.name eq 'PM_I02362+0652' then rem = 'B'
  if r.name eq 'PM_I08105-1348' then rem = 'B'
  if r.name eq 'PM_I17355+6140' then rem = 'C'
  if r.name eq 'PM_I20407+1954' then rem = 'B'
  if r.name eq 'PM_I22524+0954' then rem = 'B'
  if r.name eq 'PM_I06523-0511' then rem = 'B'
  if r.name eq 'PM_I23318+1956E' then rem = 'B'
  if r.name eq 'PM_I01186-0052S' then rem = 'B'
  if r.name eq 'LSPM_J0045+0015N' then rem = 'B'
  if r.name eq 'PM_I01076+2257E' then rem = 'B'
  if r.name eq 'Gl166C' then rem = 'C'
  if r.name eq 'PM_I11055+4331' then rem = 'BV'
  if r.name eq 'PM_I16555-0823' then rem = 'D'
  if r.name eq 'GJ_105A' then rem = 'AP'
  if r.cns3 eq 'GJ 566A' or r.name eq 'GJ 566A' then rem = 'ABV'
  if r.cns3 eq 'Gl490A' then rem = 'AV'
  if r.cns3 eq 'Gl490B' then rem = 'B'
  if r.name eq 'PM_I12576+3513W' then rem = 'B'
  if r.name eq 'PM_I12576+3513E' then rem = 'AV'
  if r.name eq 'HD_160934AB' then rem = 'A'
  print,r.cns3,' ',r.othername,' ',r.name
  
  create_photometry_file,r,redo,rem    ;; create the fancy photometry file
  
  satisfied = 0
  counter = 0
  r_fix = 1.0
  b_fix = 1.0
  r_fix_err = 99.0
  b_fix_err = 99.0
  k_fix = 1.0
  k_fix_err = 99.0
  ;;if r.cns3 eq 'GJ 570A' then b_fix = 0.89
  adjust_spectrum,r,b_fix,b_fix_err,r_fix,r_fix_err,k_fix,k_fix_err,stis=stis
  add_missing_spectrum,r,add=add,compvalues=compvalues,hot=hot
  if add eq 1 then print,compvalues[6]
  get_spectrum,r,lambda,spec,err,stis=stis

  ;; cutoff some junk
  ll = where(lambda lt 30d0)
  lambda = lambda[ll]
  spec = spec[ll]
  err = err[ll]

  counter = 0
  while satisfied eq 0 do begin
     offset_thresh = 5 ;; 5
     if r.name eq 'USCOCTIO5' then offset_thresh = 500
     counter++
     if counter eq 5 then satisfied = 1
     ;; now everything is in the file, read in that file
     readinphot,r,lambda,spec,err,phot_systems,phot_system,phot_band,phfluxes,phfluxes_err,corr,corr_err,fluxes,fluxes_err,lams,fwhms

     ;; calculation of mastercorr and error
     l = where(phot_band ne 'U' and (phot_band ne 'I' or phot_system ne 'Johnson') and phot_system ne 'Wise')
     mastercorr = median(corr[l])
     offset = abs(corr-mastercorr)/sqrt(corr_err^2.0) ;+mastercorr_Err^2.0)
     l = where(phot_band ne 'U' and (phot_band ne 'I' or phot_system ne 'Johnson') and phot_system ne 'Wise' and offset lt offset_thresh,COMPLEMENT=m)
     mastercorr = wmean(corr[l],corr_err[l],error=mastercorr_Err)
     offset = abs(corr-mastercorr)/sqrt(corr_err^2.0) ;+mastercorr_Err^2.0)
     l = where(phot_band ne 'U' and (phot_band ne 'I' or phot_system ne 'Johnson') and phot_system ne 'Wise' and offset lt offset_thresh,COMPLEMENT=m) ;  and phot_system ne 'Hipparcos'
     mastercorr = wmean(corr[l],corr_err[l],error=mastercorr_Err)

     ;; here's where we test to see if there's a problem with blue, red, or NIR regions: 
     blue = where(phot_band ne 'U' and phot_band ne 'L' and phot_system ne 'Wise' and offset lt offset_thresh and lams lt 0.52)
     red =  where(phot_band ne 'U'  and (phot_band ne 'I' or phot_system ne 'Johnson') and phot_band ne 'L' and phot_system ne 'Wise' and offset lt offset_thresh and lams gt 0.52 and lams lt 0.9)
     nir =  where(phot_band ne 'L' and phot_system ne 'Wise' and offset lt offset_thresh and lams gt 0.9)
     if nir[0] eq -1 then begin
        print,'failed to find a good NIR match'
        nir =  where(phot_band ne 'L' and phot_system ne 'Wise' and lams gt 0.9)
        stop
     endif
     if blue[0] eq -1 then begin
        mastercorr_b = 1.0
        mastercorr_b_err = 10.
     endif else mastercorr_b = wmean(corr[blue],corr_err[blue],error=mastercorr_b_err)
     if red[0] eq -1 then begin
        mastercorr_r = 1.0
        mastercorr_r_err = 10.
     endif else mastercorr_r = wmean(corr[red],corr_err[red],error=mastercorr_r_err)
     mastercorr_nir = wmean(corr[nir],corr_err[nir],error=mastercorr_nir_err)

     print,'Blue Correction: '+string(mastercorr_b,format="(D6.3)")+' +/- '+string(mastercorr_b_err,format="(D5.3)")
     print,'Red Correction:  '+string(mastercorr_r,format="(D6.3)")+' +/- '+string(mastercorr_r_err,format="(D5.3)")
     print,'NIR Correction:  '+string(mastercorr_nir,format="(D6.3)")+' +/- '+string(mastercorr_nir_err,format="(D5.3)")

     ;; test to see if the errors are inconsistent at 3-sigma:
     r_b = (mastercorr_r-mastercorr_b)/sqrt(mastercorr_b_err^2.0+mastercorr_r_err^2.0)
     nir_r = (mastercorr_nir-mastercorr_r)/sqrt(mastercorr_r_err^2.0+mastercorr_nir_err^2.0)
     print,'Disagreement: ',r_b,nir_r
     if stis eq 1 then r_b = 0.

     if abs(nir_r) gt 4 then begin
        print,'Fuck'
        stop
     endif

     thresh_nir = 4 ;; maximum difference in standard deviations, important parameter
     thresh_b = 2.0 ;; maximum difference in standard deviations, blue
     if r.name eq 'PM_I16148+6038' then thresh_b = 1.5
     ;;if r.name eq 'USCOCTIO5' then thresh_nir = 150
     if abs(nir_r) gt thresh_nir or abs(r_b) gt thresh_b then begin
        if abs(nir_r) gt thresh_nir then begin
           r_fix*=(mastercorr_r/mastercorr_nir)
           r_fix_err = r_fix*sqrt((mastercorr_r_err/mastercorr_r)^2.+(mastercorr_nir_err/mastercorr_nir)^2.)
        endif
        if abs(r_b) gt thresh_b then begin
           b_fix*=(mastercorr_b/mastercorr_r)
           b_fix_err = b_fix*sqrt((mastercorr_b_err/mastercorr_b)^2.+(mastercorr_r_err/mastercorr_r)^2.)
        endif
        ll = where(phot_band eq 'K')
        if ll[0] ne -1 then begin
           if n_elements(ll) eq 1 then begin
              k_fix = corr[ll]
              k_fix_err = corr_Err[ll]
           endif else begin
              k_fix = wmean(corr[ll],corr_err[ll],error=k_fix_err)
           endelse
        endif else begin
           k_fix = 1.0
           k_fix_err = 99.0
        endelse
        adjust_spectrum,r,b_fix,b_fix_err,r_fix,r_fix_err,k_fix,k_fix_err
        add_missing_spectrum,r,add=add,hot=hot
        get_spectrum,r,lambda,spec,err,stis=stis
        ;; cutoff some junk 
        ll = where(lambda lt 30d0)
        lambda = lambda[ll]
        spec = spec[ll]
        err = err[ll]
     endif else satisfied = 1
     counter++
     if counter gt 10 then begin
        print,'Failed to converge on reasonable solution'
        stop
     endif
  endwhile
  print,'Final Master correction: '+string(mastercorr,format="(D6.3)")+' +/- '+string(mastercorr_err,format="(D6.3)")
  spec = spec*mastercorr
  err = err*mastercorr

  ;; now fix the RJ tail
  ;; what we want to do is do a RJ fit but also include the photomeric points
  ;; another option is to try extrapolating the models all the way out.
  ;;print,'Fitting Rayleigh-Jeans Tail'
  set_plot,'x'
  !p.multi=[0,1,1]
  q = where(lams gt 2.5)
  if q[0] ne -1 and max(lambda) lt 29d0 then begin
     ;; clear out the old shitty RJ fit
     g = where(lambda lt 2 or err ne 0)
     spec = spec[g]
     lambda = lambda[g]
     err = err[g]
     ;; now for a new fit!
     f = where(lambda gt 2.6 and err ne 0 and spec)
     if f[0] eq -1 then f = where(lambda gt 2.0 and err ne 0)
     guess = [1d-10,5]
     result = mpfitfun('MYRJL',[lambda[f],lams[q]],[spec[f],phfluxes[q]],[spec[f]/2.,phfluxes_err[q]],guess,perror=perr,/quiet)
     newlambda = generatearray(max(lambda),30.0,100)
     newspec = myrjl(newlambda,result)
     newerr = 0.0*((newspec)*(perr[0]/result[0]))
     ;;plot,lambda,spec,xrange=[2,4]
     ;;oplot,newlambda,newspec,color=cgcolor('red')

     lambda = [lambda,newlambda]
     spec = [spec,newspec]
     err = [err,newerr]
     plot,lambda[where(spec gt 0)],spec[where(spec gt 0)],xrange=[2,30],/xlog,/ylog,/xstyle
     oploterror,lams[q],phfluxes[q],fwhms[q],phfluxes_err[q],psym=8
     off = dblarr(n_elements(q))
     for jj = 0,n_elements(q)-1 do begin
        get_flux,phot_systems,phot_system[q[jj]],phot_band[q[jj]],lambda,spec,err,flux,flux_err
        oplot,[lams[q[jj]]],[flux],psym=8
        off[jj] = (phfluxes[q[jj]]-flux)/phfluxes_err[q[jj]]
     endfor
     f = where(lambda gt 2.6 and err ne 0)
     guess = [1.0,-1.0]
     newlambda = generatearray(max(lambda[where(err ne 0)]),30.0,100)
     nmonte = 500
     bigarr = dblarr(n_elements(newlambda),nmonte)
     newerr = dblarr(n_elements(newlambda))
     for iii = 0,nmonte-1 do begin
        res_temp = result+perr*randomn(seed,2)
        bigarr[*,iii] = MYRJL(newlambda,res_temp)
     endfor
     for jjj = 0,n_elements(newlambda)-1 do begin
        newerr[jjj] = stdev(bigarr[jjj,*])
     endfor
     g = where(err eq 0 and lambda gt 2.0)
     err[g] = newerr   
  endif

  ;; now fix the Wein tail
  ;;print,'Fitting Wein Tail'
  if stis eq 0 then begin ;; do not fit if stis spectrum
     satisfied = 0
     counter = 0
     q = where(lams lt 0.37)
     if n_elements(q) gt 2 then begin ;; need 3 for statistics
        while satisfied eq 0 do begin
           ;; test the Wein fit
           plot,lambda,spec,xrange=[0.3,0.4]
           oploterror,lams[q],phfluxes[q],fwhms[q],phfluxes_err[q],psym=8
           off = dblarr(n_elements(q))
           frac = off
           for jj = 0,n_elements(q)-1 do begin
              get_flux,phot_systems,phot_system[q[jj]],phot_band[q[jj]],lambda,spec,err,flux,flux_err
              oplot,[lams[q[jj]]],[flux],psym=8
              off[jj] = (phfluxes[q[jj]]-flux)^2.0/(phfluxes_err[q[jj]]^2.0+flux_err^2.0)
              frac[jj] = phfluxes[q[jj]]/flux
           endfor
           if median(off) gt offset_thresh then begin
              counter++
              spec[where(lambda lt 0.4 and err eq 0)]*=sqrt((median(frac)))
              satisfied = 0
           endif else begin
              counter++
              spec[where(lambda lt 0.4 and err eq 0)]*=sqrt((median(frac)))
              satisfied = 1
           endelse
           if counter gt 3 then begin
              print,'Failed to converge on Wein'
              satisfied = 1
           endif
           if max(spec[where(lambda lt 0.4 and err eq 0)]) lt min(spec[where(lambda lt 0.4 and err ne 0)]) then satisfied = 1
           if max(spec[where(lambda lt 0.4 and err eq 0)]) gt max(spec[where(lambda lt 0.36 and err ne 0)]) then satisfied = 1
        endwhile
     endif
     f = where(lambda lt 0.5 and err ne 0.0)
     guess = [1.0,-1.0]
     result = mpfitfun('MYWEIN',lambda[f],spec[f],err[f],guess,/quiet,PERROR=weinerr)
     newlambda = generatearray(0.25,min(lambda[where(err ne 0)]),100)
     nmonte = 250
     bigarr = dblarr(n_elements(newlambda),nmonte)
     newerr = dblarr(n_elements(newlambda))
     for iii = 0,nmonte-1 do begin
        res_temp = result+weinerr*randomn(seed,2)
        bigarr[*,iii] = mywein(newlambda,res_temp)
     endfor
     for jjj = 0,n_elements(newlambda)-1 do begin
        newerr[jjj] = stdev(bigarr[jjj,*])
     endfor
     g = where(err eq 0 and lambda lt 0.5)
     err[g] = newerr     
  endif

  ;; re-extract all synthetic photometry:
  fluxes = dblarr(n_elements(phot_system))
  fluxes_err = fluxes
  for jj = 0,n_elements(phot_system)-1 do begin
     get_flux,phot_systems,phot_system[jj],phot_band[jj],lambda,spec,err,flux,flux_err
     fluxes[jj] = flux
     fluxes_err[jj] = flux_err
  endfor
  readinphot,r,lambda,spec,err,phot_systems,phot_system,phot_band,phfluxes,phfluxes_err,corr,corr_err,fluxes,fluxes_err,lams,fwhms
  
  ;; calculation of mastercorr and error
  test_corr = median(corr)
  offset = abs(test_corr-corr)/sqrt(corr_err^2.0) ;+mastercorr_Err^2.0)
  l = where(offset lt offset_thresh and corr_err lt 1,COMPLEMENT=m)
  test_corr2 = wmean(corr[l],corr_err[l],error=test_corr_Err)
  offset = abs(test_corr2-corr)/sqrt(corr_err^2.0) ;+mastercorr_Err^2.0)
  l = where(offset lt offset_thresh,COMPLEMENT=m)
  l_tmp = where((offset lt offset_thresh and offset gt 0) or phot_system eq 'Wise',COMPLEMENT=m2)
  l_nowise = where(offset lt offset_thresh and phot_system ne 'Wise'  and phot_system ne 'Spitzer')
  l_cover = where(offset lt offset_thresh and phot_system ne 'Wise' and phot_system ne 'Spitzer' and lams-fwhms gt 0.31)
  rchisq = total((corr[l]-test_corr2)^2.0/corr_err[l]^2.0)/(n_elements(l)-2)
  rchisq_nowise = total((corr[l_nowise]-test_corr2)^2.0/corr_err[l_nowise]^2.0)/(n_elements(l_nowise)-2)
  rchisq_cover = total((corr[l_cover]-test_corr2)^2.0/corr_err[l_cover]^2.0)/(n_elements(l_cover)-2)
  forprint, string(phot_system[l],format="(A10)")+string(9b)+string(phot_band[l],format="(A4)")+string(9b)+string(lams[l],format="(D5.2)"),$
            (corr[l]-test_corr)/corr_err[l], corr[l],corr_err[l], textout='phots/'+name+'.phot',comment='-----------------Goddies-----------------',/silent
  if m[0] ne -1 then $
     forprint,string(phot_system[m],format="(A10)")+string(9b)+string(phot_band[m],format="(A4)")+string(9b)+string(lams[m],format="(D5.2)"),$
              (corr[m]-test_corr)/corr_err[m],(corr[m]),corr_err[m],textout='phots/'+name+'.badphot',comment='-----------------Baddies-----------------',/silent
  print,'Final Secondary Correction: '+string(test_corr2,format="(D6.3)") +' +/- '+string(test_corr_err,format="(D6.3)")
  if m[0] eq -1 then count = 0 else count = n_Elements(m)
  if m2[0] eq -1 then count2 = 0 else count2 = n_Elements(m2)
  print,'Final Reduced Chi^2: '+string(rchisq,format="(D5.2)")+' with '+string(n_elements(l),format="(I3)")+' photometric points.  '+string(count,format="(I3)")+' points excluded. '+string(count2,format="(I2)")+' non-Wise non-I,R points excluded.' 
  print,'Final Reduced Chi^2 without wise: '+string(rchisq_nowise,format="(D5.2)")
  print,'Final Reduced Chi^2 points covered: '+string(rchisq_cover,format="(D5.2)")
  if m[0] ne -1 then print,phot_system[m]
  if m[0] ne -1 then print,phot_band[m]
  goodpoints = n_elements(l_nowise)

  ;;;
  if test_corr2 gt 0 then spec*=test_corr2
  if r.cns3 eq 'GJ 380' or r.cns3 eq 'GJ 436' or r.cns3 eq 'GJ 725A' then spec*=1.04 ;; THESE NEED FIXING
  if r.cns3 eq 'GJ 380' then spec*=1.01

  gg = where(err ne 0)
  medsnr = median(spec[gg]/err[gg])
  loc = where(finite(spec) eq 1 and finite(lambda) eq 1)
  fbol = integral(lambda[loc],spec[loc])*1d12

  nmonte = 1d3
  fbols = dblarr(nmonte)
  for kk = 0d0,nmonte-1d0 do begin
     tmp_spec = spec+err*randomn(seed,n_elements(spec))
     tmp_spec*=(1.+(randomn(seed)*test_corr_err))
     jj = where(finite(tmp_spec) eq 1 and finite(lambda) eq 1)
     fbols[kk] = integral(lambda[jj],tmp_spec[jj])
  endfor
  fbol_err = stdev(fbols)*1d12
  ;; add 1.5% systematic error
  fbol_err=sqrt(fbol_err^2.+(0.015*fbol)^2.)
  ;; scale by rchisq
  if rchisq gt 1d0 then fbol_err*=sqrt(rchisq)
  print,'Final Fbol: '+string(fbol,format="(D9.5)")+' +/- '+string(fbol_err,format="(D9.5)")
  ;; save the adjusted spectrum
  finalspec = spec
  finallambda = lambda
  finalerr = err
  finalfbol = fbol
  finalfbol_err = fbol_err


  ;; Plot!
  set_plot,'x'
  device
  set_plot,'PS'
  gcal_plotter,r,phot_system,offset,offset_thresh,name,lambda,spec,err,lams,phfluxes,phfluxes_err,fwhms,FLUXES,FLUXES_err,add=add,inset=inset,long=long,oc=oc,e_corr=corr_err,corr=corr,printname=printname
  ;; output for kaspar
  n = r.cns3
  if strtrim(n,2) eq '' then n = r.name
  kaspar_format,n,finallambda,finalspec,finalerr,add=add


END


PRO adjust_spectrum,h,op_adjust,op_adjust_err,IR_adjust,ir_adjust_err,k_fix,k_fix_err,stis=stis
  
  if n_elements(k_fix) eq 0 or n_elements(k_fix_err) eq 0 then begin
     k_fix = 1.0
     k_fix_err = 99.
  endif
  COMPILE_OPT idl2, HIDDEN
  set_plot,'x'
  !p.multi=[0,1,2]
  snifsfwhm = (7000.0/1000.0)
  c = 299792.458D

  extractdata,h,opspec,operr,oplambda,opspec_b,operr_b,oplambda_b,oprv,irspec,irerr,irlambda,irrv,stis=stis
  
  ;;if h.cns3 eq 'GJ 725B' then irspec[where(irlambda gt 1.9)]*=0.98
  ;;if h.cns3 eq 'GJ 725A' then irspec[where(irlambda gt 1.9)]*=0.98
  if h.name eq 'HD_189733' then irspec[where(irlambda gt 1.9)]*=0.94
  if h.name eq 'PM_I01186-0052S' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I01402+3147' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I01056+2829' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I04290+2155' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I14201-0937' then irspec[where(irlambda gt 1.9)]*=0.93
  if h.name eq 'PM_I14342-1231' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I16254+5418' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I16509+2227' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I19220+0702' then irspec[where(irlambda gt 1.9)]*=0.96
  if h.name eq 'PM_I20034+2951' then irspec[where(irlambda gt 1.9)]*=0.93
  if h.name eq 'PM_I20260+5834' then irspec[where(irlambda gt 1.9)]*=0.90
  if h.name eq 'LSPM_J0355+5214' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'GJ673' then irspec[where(irlambda gt 1.9)]*=0.93
  if h.name eq 'PM_I06011+5935' then irspec[where(irlambda gt 1.9)]*=1.05
  if h.name eq 'PM_I09095+3249N' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I09437-1747' then irspec[where(irlambda gt 1.9)]*=1.05
  if h.name eq 'PM_I12388+1141' then irspec[where(irlambda gt 1.9)]*=1.05
  if h.name eq 'PM_I13007+1222' then irspec[where(irlambda gt 1.9)]*=0.96
  if h.name eq 'PM_I00183+4401' then irspec[where(irlambda gt 1.9)]*=1.075
  if h.name eq 'PM_I18498-2350' then irspec[where(irlambda gt 1.9)]*=0.98
  if h.name eq 'J04130515145' then irspec[where(irlambda gt 1.9)]*=1.04
  if h.name eq 'PM_I01036+4051' then irspec[where(irlambda gt 1.9)]*=1.1
  if h.name eq 'J03104941-3616471' then irspec[where(irlambda gt 1.9)]*=0.94
  if h.name eq 'LSPM_J2214+2534' then  irspec[where(irlambda gt 1.9)]*=1.05
  if h.name eq '00354313+0233137' then irspec[where(irlambda gt 1.9)]*=0.92
  if h.name eq '03363144-2619578' then  irspec[where(irlambda gt 1.9)]*=0.88
  if h.name eq 'EPIC_211901114' then  irspec[where(irlambda gt 1.9)]*=1.02
  if h.name eq 'EPIC_211916756' then  irspec[where(irlambda gt 1.9)]*=0.93
  if h.name eq 'EPIC_211969807' then irspec[wherE(irlambda gt 1.9)]*=1.07
  ;;if h.name eq '205117205' then  irspec[where(irlambda gt 1.9)]*=1.02
  ;;if h.name eq 'PM_I15238+1727' then irspec[where(irlambda gt 1.9)]*=0.98
  if k_fix ne 1. and abs((k_fix-1.0)/k_fix_err) gt 1 then begin
     if k_fix gt 1.02 then k_fix = 1.02
     if k_fix lt 0.98 then k_fix = 0.98
     irspec[where(irlambda gt 1.9)]*=k_fix[0]
     irspec[where(irlambda gt 1.9)]*=k_fix[0]
  endif
  
  ;; apply RV corrections
  irlambda = irlambda/(irrv/c+1.)

  opoverlaplambda = oplambda[where(oplambda gt min(irlambda))]
  iroverlaplambda = irlambda[where(irlambda lt max(oplambda))]
  opoverlap = opspec[where(oplambda gt min(irlambda))]
  iroverlap = irspec[where(irlambda lt max(oplambda))]
  opoverlap_err = operr[where(oplambda gt min(irlambda))]
  iroverlaplambda_err = irerr[where(irlambda lt max(oplambda))]

  compspec = interpol(iroverlap,iroverlaplambda,opoverlaplambda) ;; for now we interpolate the IR to the optical
  l = opoverlaplambda
  limits = [0.805,0.90]
  if h.name eq 'GJ_105A' then limits = [0.92,0.94]
  compute_xcor_rv, l, opoverlap,'M1.5', rv, wavelim=[min(l),max(l)],norm_reg=limits,twave=l, tflux=compspec, maxshift=20,showplot=0
  if h.name eq 'LSPM_J0315+0103' then rv = 0.0
  if h.name eq 'NLTT37349' then rv = 0.0
  oplambda = oplambda/(rv/c+1.)
  oplambda_b = oplambda_b/(rv/c+1.)


  ;; limits = [0.805,0.94]
  ;; right = irspec[where(irlambda gt limits[0] and irlambda lt limits[1])]
  ;; right_err = irerr[where(irlambda gt limits[0] and irlambda lt limits[1])]
  ;; right_lambda = irlambda[where(irlambda gt limits[0] and irlambda lt limits[1])]
  ;; s1 = median(right)
  ;; left = opspec[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; left_err = operr[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; left_lambda = oplambda[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; right = interpol(right,right_lambda,left_lambda)
  ;; s2 = median(left)
  ;; change = right/left
  ;; if ir_adjust ne 1 or op_adjust ne 1 then stop


  limits = [0.805,0.88]
  if h.cns3 eq 'GJ 176' then limits = [0.805,0.84]
  if h.name eq 'GJ_105A' then limits = [0.92,0.94] ;limits = [0.86,0.90]
  if h.name eq 'LSPM_J2106+3844S' then limits = [0.81,0.86]
  if h.cns3 eq 'GJ 338A' then limits = [0.805,0.85]
  if h.cns3 eq 'GJ 411' then limits = [0.92,0.94]
  if h.cns3 eq 'GJ 526' then limits = [0.90,0.94]
  if h.cns3 eq 'GJ 15B' then limits = [0.86,0.90]
  if h.name eq 'PM_I11033+3558' then limits = [0.90,0.92]
  if h.name eq 'PM_I01324-2154' then limits = [0.94,0.96]
  if h.name eq 'PM_I01510-0607' then limits = [0.85,0.90]
  if h.name eq 'PM_I02050-1736' then limits = [0.88,0.93]
  if h.name eq 'PM_I02123+0334' then limits = [0.86,0.91]
  if h.name eq 'PM_I04376+5253' then limits = [0.88,0.92]
  if h.name eq 'PM_I04429+1857' then limits = [0.805,0.85]
  if h.name eq 'PM_I05314-0340' then limits = [0.88,0.92]
  if h.name eq 'PM_I05557-2651' then limits = [0.86,0.92]
  if h.name eq 'PM_I06140+5140' then limits = [0.88,0.94]
  if h.name eq 'PM_I06246+2325' then limits = [0.83,0.86]
  if h.name eq 'PM_I07232+4605' then limits = [0.85,0.89]
  if h.name eq 'PM_I07274+0513' then limits = [0.88,0.93]
  if h.name eq 'PM_I00183+4401' then limits = [0.88,0.92]
  if h.name eq 'PM_I05033-1722' then limits = [0.88,0.92]
  if h.name eq 'PM_I07287-0317' then limits = [0.86,0.90]
  if h.name eq 'PM_I10196+1952' then limits = [0.88,0.92]
  if h.name eq 'PM_I10520+1359' then limits = [0.88,0.94]
  if h.name eq 'PM_I13457+1453' then limits = [0.88,0.94]
  if h.name eq 'PM_I17364+6820' then limits = [0.805,0.86]
  if h.name eq 'PM_I19070+2053' then limits = [0.88,0.94]
  if h.name eq 'PM_I19169+0510' then limits = [0.83,0.85]
  if h.name eq 'PM_I20525-1658' then limits = [0.88,0.92]
  if h.name eq 'PM_I20533+6209' then limits = [0.83,0.86]
  if h.name eq 'PM_I22361-0050' then limits = [0.83,0.85]
  if h.name eq 'PM_I22532-1415' then limits = [0.9,0.93]
  if h.name eq 'PM_I22565+1633' then limits = [0.855,0.89]
  if h.name eq 'PM_I23455-1610' then limits = [0.89,0.94]
  if h.name eq 'PM_I23492+0224' then limits = [0.89,0.94]
  if h.name eq 'PM_I06490+3706' then limits = [0.86,0.91]
  if h.name eq 'PM_I22290+0139' then limits = [0.89,0.94]
  if h.name eq 'PM_I22021+0124' then limits = [0.87,0.89]
  if h.name eq 'PM_I00219-3124' then limits = [0.87,0.92]
  if h.name eq 'PM_I01571-1014E' then limits = [0.84,0.87]
  if h.name eq 'PM_I08105-1348' then limits = [0.88,0.94]
  if h.name eq 'PM_I10304+5559' then limits = [0.88,0.94]
  if h.name eq 'PM_I17052-0505' then limits = [0.88,0.94]
  if h.name eq 'PM_I06523-0511' then limits = [0.87,0.91]
  if h.name eq 'PM_I23318+1956W' then limits = [0.87,0.92]
  if h.name eq 'HD_209458' then limits = [0.9,0.92]
  if h.name eq 'PM_I01056+2829' then limits = [0.82,0.85]
  if h.name eq 'PM_I01186-0052S' then limits = [0.83,0.85]
  if h.name eq 'PM_I01433+0419' then limits = [0.83,0.85]
  if h.name eq 'PM_I01402+3147' then limits = [0.72,0.75]
  if h.name eq 'PM_I02164+1335' then limits = [0.90,0.94]
  if h.name eq 'PM_I02358+2013' then limits = [0.88,0.92]
  if h.name eq 'PM_I03095+4543' then limits = [0.88,0.92]
  if h.name eq 'PM_I03181+3815' then limits = [0.88,0.92]
  if h.name eq 'PM_I04290+2155' then limits = [0.88,0.92]
  if h.name eq 'PM_I05421+1229' then limits = [0.88,0.92]
  if h.name eq 'PM_I16254+5418' then limits = [0.855,0.865]
  if h.name eq 'PM_I17095+4340' then limits = [0.88,0.92]
  if h.name eq 'PM_I17198+4142' then limits = [0.85,0.87]
  if h.name eq 'PM_I18453+1851' then limits = [0.88,0.92]
  if h.name eq 'PM_I19220+0702' then limits = [0.88,0.92]
  if h.name eq 'PM_I19396-2645' then limits = [0.86,0.89]
  if h.name eq 'PM_I19539+4424W' then limits = [0.88,0.93]
  if h.name eq 'PM_I20034+2951' then limits = [0.88,0.93]
  if h.name eq 'PM_I20105+0632' then limits = [0.89,0.92]
  if h.name eq 'PM_I20260+5834' then limits = [0.87,0.92]
  if h.name eq 'PM_I22279+5741W' then limits = [0.87,0.92]
  if h.name eq 'PM_I16241+4821' then limits = [0.86,0.88]
  if h.name eq 'PM_I22374+3922' then limits = [0.88,0.92]
  if h.name eq 'PM_I23428+3049' then limits = [0.88,0.92]
  if h.name eq 'PM_I23216+1717' then limits = [0.85,0.88]
  if h.name eq 'PM_I21000+4004E' then limits = [0.88,0.95]
  if h.name eq 'PM_I23318+1956E' then limits = [0.90,0.95]
  if h.name eq 'PM_I22468+4420' then limits = [0.88,0.92]
  if h.name eq 'PM_I11054+4331' then limits = [0.88,0.92]
  if h.name eq 'PM_I07344+6256' then limits = [0.89,0.93]
  if h.name eq 'PM_I02002+1303' then limits = [0.90,0.95]
  if h.name eq 'PM_I17378+1835' then limits = [0.88,0.94]
  if h.name eq 'PM_I17578+4635' then limits = [0.88,0.92]
  if h.name eq 'PM_I16303-1239' then limits = [0.88,0.92]
  if h.name eq 'PM_I01230-1257W' then limits = [0.88,0.92]
  if h.name eq 'LSPM_J0355+5214' then limits = [0.88,0.92]
  if h.name eq 'PM_I05463+0112' then limits = [0.88,0.92]
  if h.name eq 'PM_I15354+6005' then limits = [0.88,0.92]
  if h.name eq 'PM_I18409+3131S' then limits = [0.90,0.92]
  if h.name eq 'PM_I20407+1954' then limits = [0.88,0.92]
  if h.name eq 'PM_I21518+4220E' then limits = [0.86,0.91]
  if h.name eq 'PM_I22160+5439' then limits = [0.88,0.92]
  if h.name eq 'PM_I22524+0954' then limits = [0.90,0.94]
  if h.name eq 'Gl166C' then limits = [0.88,0.92]
  if h.name eq 'PM_I17176+5224' then limits = [0.88,0.92]
  if h.name eq 'PM_I18363+1336S' then limits = [0.73,0.76]
  if h.name eq 'GJ887' then limits = [0.88,0.92]
  if h.name eq 'PM_I02190+2352' then limits = [0.73,0.75]
  if h.name eq 'PM_I14513+1906' then limits = [0.93,0.95]
  if h.name eq 'HD_103095' then limits = [0.88,0.92]
  if h.name eq 'PM_I06548+3316' then limits = [0.88,0.95]
  if h.name eq 'PM_I07446+0333' then limits = [0.88,0.94]
  if h.name eq 'PM_I08161+0118' then limits = [0.88,0.92]
  if h.name eq 'PM_I10564+0700' then limits = [0.75,0.8]
  if h.name eq 'PM_I10430-0912' then limits = [0.88,0.92]
  if h.name eq 'PM_I12388+1141' then limits = [0.88,0.92]
  if h.name eq 'PM_I15194-0743E' then limits = [0.88,0.94]
  if h.name eq 'PM_I15238+1727' then limits = [0.88,0.94]
  if h.name eq 'PM_I23419+4410' then limits = [0.88,0.94]
  if h.name eq 'PM_I09362+3731' then limits = [0.88,0.94]
  if h.name eq 'TWA_9B' then limits = [0.80,0.88]
  if h.name eq 'J04130515145' then limits = [0.8,0.88]
  if h.name eq 'PM_I02234+2244' then limits = [0.88,0.93]
  if h.name eq 'HIP_112312_B' then limits= [0.92,0.97]
  if h.name eq 'GJ2060C' then limits= [0.88,0.95]
  if h.name eq 'J00393579-3816584' then limits=[0.88,0.95]
  if h.name eq 'J03104941-3616471' then limits=[0.88,0.95]
  if h.name eq 'J20474501-3635409' then limits=[0.80,0.88]
  if h.name eq 'J22021626-4210329' then limits=[0.72,0.80]
  if h.name eq '03363144-2619578' then limits = [0.89,0.94]
  if h.name eq 'EPIC_211351816' then limits = [0.77,0.91]
  if h.name eq 'EPIC_211990866' then limits = [0.75,0.92]
  ;;s1 = median(opspec[where(oplambda gt limits[0] and oplambda lt limits[1])])
  ;;s2 = median(irspec[where(irlambda gt limits[0] and irlambda lt limits[1])])
  ;;Coverlap = s2/s1

  right = irspec[where(irlambda gt limits[0] and irlambda lt limits[1])]
  right_err = irerr[where(irlambda gt limits[0] and irlambda lt limits[1])]
  right_lambda = irlambda[where(irlambda gt limits[0] and irlambda lt limits[1])]
  s1 = median(right)
  left = opspec[where(oplambda gt limits[0] and oplambda lt limits[1])]
  left_err = operr[where(oplambda gt limits[0] and oplambda lt limits[1])]
  left_lambda = oplambda[where(oplambda gt limits[0] and oplambda lt limits[1])]
  right = interpol(right,right_lambda,left_lambda)
  s2 = median(left)
  change = right/left
  coverlap_base = median(change)
  coverlap_base_err = sqrt(stdev(change)^2.0+(0.02*coverlap_base)^2.0) ;; 1-2% is the error in the flux cal
  coverlap_other = coverlap_base*ir_adjust
  coverlap_other_err = ir_adjust_err*coverlap_base
  if ir_adjust_err lt 1 then coverlap = wmean([coverlap_base,coverlap_other],[coverlap_base_err,coverlap_other_err]) else coverlap = coverlap_base
  
  opspec*=Coverlap
  operr*=Coverlap
  print,'Red correction  ',coverlap

  ;; same for blue/red channel
  limits2 = [0.510,0.515]
  if h.name eq 'PM_I13457+1453' then limits = [0.50,0.511]
  if h.name eq 'PM_I04073-2429' then limits2 = [0.5,0.512]
  right = opspec[where(oplambda gt limits2[0] and oplambda lt limits2[1])]
  right_err = operr[where(oplambda gt limits2[0] and oplambda lt limits2[1])]
  right_lambda = oplambda[where(oplambda gt limits2[0] and oplambda lt limits2[1])]
  s1 = median(right)
  left = opspec_b[where(oplambda_b gt limits2[0] and oplambda_b lt limits2[1])]
  left_err = operr_b[where(oplambda_b gt limits2[0] and oplambda_b lt limits2[1])]
  left_lambda = oplambda_b[where(oplambda_b gt limits2[0] and oplambda_b lt limits2[1])]
  right = interpol(right,right_lambda,left_lambda)
  s2 = median(left)
  Coverlap = s1/s2

  
  change = right/left
  coverlap_base = median(change)
  coverlap_base_err = sqrt(stdev(change)^2.0+(0.03*coverlap_base)^2.0) ;; 1-2% is the error in the flux cal
  coverlap_other = coverlap_base*op_adjust
  coverlap_other_err = op_adjust_err*coverlap_base
  if op_adjust_err lt 1 then coverlap = wmean([coverlap_base,coverlap_other],[coverlap_base_err,coverlap_other_err]) else coverlap = coverlap_base

  if h.name eq 'PM_I03361+3118' then coverlap*=1.1
  if h.cns3 eq 'GJ 702B' then coverlap*=1.1
  if h.cns3 eq 'GJ 876' then coverlap*=1.05 ;1.15
  if h.cns3 eq 'GJ 649' then coverlap*=0.93
  if h.name eq 'PM_I01528-2226' then coverlap*=1.05
  if h.name eq 'PM_I04073-2429' then coverlap *= 0.89
  if h.name eq 'PM_I04376-1102' then coverlap *= 1.05
  if h.name eq 'PM_I06077-2544' then coverlap *= 1.05
  if h.name eq 'PM_I13457+1453' then coverlap *= 1.25
  if h.name eq 'PM_I16570-0420' then coverlap *=0.85
  if h.name eq 'PM_I20533+6209' then coverlap *=0.85
  if h.name eq 'PM_I23455-1610' then coverlap *=0.90
  if h.name eq 'PM_I23318+1956W' then coverlap *=0.83
  if h.name eq 'PM_I01056+2829' then coverlap *=0.80
  if h.name eq 'PM_I00115+5908' then coverlap *=1.2
  if h.name eq 'PM_I00118+2259' then coverlap*=0.95
  if h.name eq 'PM_I01186-0052S' then coverlap*=1.07
  if h.name eq 'PM_I02164+1335' then coverlap*=0.72
  if h.name eq 'PM_I05421+1229' then coverlap*=1.25
  if h.name eq 'PM_I17095+4340' then coverlap*=0.95
  if h.name eq 'PM_I17115+3826' then coverlap*=0.9
  if h.name eq 'PM_I18453+1851' then coverlap*=0.85
  if h.name eq 'PM_I19220+0702' then coverlap*=0.95
  if h.name eq 'PM_I19539+4424W' then coverlap*=0.9
  if h.name eq 'PM_I20034+2951' then coverlap*=0.93
  if h.name eq 'PM_I20105+0632' then coverlap*=1.25
  if h.name eq 'PM_I20450+4429' then coverlap*=1.1
  if h.name eq 'PM_I06140+5140' then coverlap*=0.9
  if h.name eq 'PM_I09447-1812' then coverlap*=0.95
  if h.name eq 'PM_I18363+1336S' then coverlap*=1.2
  if h.name eq 'PM_I08298+2646' then coverlap*=0.3
  if h.name eq 'J04130515145' then coverlap*=0.93 ;0.98
  if h.name eq 'PM_I13143+1320' then coverlap*=0.93
  if h.name eq 'EPIC_210363145' then coverlap*=0.74
  if h.name eq 'HIP_112312_B' then coverlap*=0.96
  if h.name eq 'HIP_106231' then coverlap*=0.55
  if h.name eq 'J20474501-3635409' then coverlap*=0.9
  if h.name eq 'EPIC_211351816' then coverlap*=0.7
  if h.name eq 'EPIC_211901114' then coverlap*=0.65
  if h.name eq 'EPIC_211969807' then coverlap*=0.65
  ;;if h.name eq 'PM_I23492+0224' then coverlap *=0.90
  ;;coverlap*=op_adjust
  if stis eq 0 then begin
     opspec_b*=Coverlap
     operr_b*=Coverlap
  endif
  set_plot,'x'
  plot,oplambda,opspec,xrange=[0.475,0.55]
  if stis eq 0 then begin
     oplot,oplambda_b,opspec_b,color=cgcolor('blue')
     print,'Blue correction  ',coverlap
  endif

  limits = [0.83,0.9]                              ;;[0.925,0.94];;
  if h.name eq 'GJ_105A' then limits = [0.92,0.94] ;limits = [0.86,0.90]
  if h.name eq 'PM_I11033+3558' then limits = [0.90,0.92]
  if h.name eq 'LSPM_J2106+3844S' then limits = [0.81,0.86]
  if h.cns3 eq 'GJ 338A' then limits = [0.805,0.85]
  if h.cns3 eq 'GJ 411' then limits = [0.92,0.94]
  if h.cns3 eq 'GJ 526' then limits = [0.90,0.94]
  if h.cns3 eq 'GJ 2' then limits = [0.83,0.89]
  if h.cns3 eq 'GJ 15B' then limits = [0.86,0.90]
  if h.cns3 eq 'GJ 1009' then limits = [0.805,0.89]
  if h.name eq 'PM_I01324-2154' then limits = [0.94,0.96]
  if h.name eq 'PM_I01510-0607' then limits = [0.85,0.90]
  if h.name eq 'PM_I02050-1736' then limits = [0.88,0.93]
  if h.name eq 'PM_I02123+0334' then limits = [0.86,0.91]
  if h.name eq 'PM_I04376+5253' then limits = [0.88,0.92]
  if h.name eq 'PM_I04429+1857' then limits = [0.805,0.85]
  if h.name eq 'PM_I05314-0340' then limits = [0.88,0.92]
  if h.name eq 'PM_I05557-2651' then limits = [0.86,0.92]
  if h.name eq 'PM_I06140+5140' then limits = [0.88,0.94]
  if h.name eq 'PM_I06246+2325' then limits = [0.83,0.86]
  if h.name eq 'PM_I07232+4605' then limits = [0.85,0.89]
  if h.name eq 'PM_I07274+0513' then limits = [0.88,0.93]
  if h.name eq 'PM_I00051+4547' then limits = [0.83,0.85]
  if h.name eq 'PM_I00183+4401' then limits = [0.88,0.92]
  if h.name eq 'PM_I05033-1722' then limits = [0.88,0.92]
  if h.name eq 'PM_I07287-0317' then limits = [0.86,0.90]
  if h.name eq 'PM_I10196+1952' then limits = [0.88,0.92]
  if h.name eq 'PM_I10520+1359' then limits = [0.88,0.94]
  if h.name eq 'PM_I13457+1453' then limits = [0.88,0.94]
  if h.name eq 'PM_I17364+6820' then limits = [0.805,0.86]
  if h.name eq 'PM_I19070+2053' then limits = [0.88,0.94]
  if h.name eq 'PM_I19169+0510' then limits = [0.83,0.85]
  if h.name eq 'PM_I20525-1658' then limits = [0.88,0.92]
  if h.name eq 'PM_I20533+6209' then limits = [0.83,0.86]
  if h.name eq 'PM_I22361-0050' then limits = [0.83,0.85]
  if h.name eq 'PM_I22532-1415' then limits = [0.9,0.93]
  if h.name eq 'PM_I22565+1633' then limits = [0.855,0.89]
  if h.name eq 'PM_I23455-1610' then limits = [0.89,0.94]
  if h.name eq 'PM_I23492+0224' then limits = [0.89,0.94]
  if h.name eq 'PM_I06490+3706' then limits = [0.86,0.91]
  if h.name eq 'PM_I22290+0139' then limits = [0.89,0.94]
  if h.name eq 'PM_I22021+0124' then limits = [0.87,0.89]
  if h.name eq 'PM_I00219-3124' then limits = [0.87,0.92]
  if h.name eq 'PM_I01571-1014E' then limits = [0.84,0.87]
  if h.name eq 'PM_I08105-1348' then limits = [0.88,0.94]
  if h.name eq 'PM_I10304+5559' then limits = [0.88,0.94]
  if h.name eq 'PM_I17052-0505' then limits = [0.88,0.94]
  if h.name eq 'PM_I06523-0511' then limits = [0.87,0.91]
  if h.name eq 'PM_I23318+1956W' then limits = [0.87,0.92]
  if h.name eq 'HD_209458' then limits = [0.9,0.92]
  if h.name eq 'PM_I01056+2829' then limits = [0.82,0.85]
  if h.name eq 'PM_I01186-0052S' then limits = [0.83,0.85]
  if h.name eq 'PM_I01433+0419' then limits = [0.83,0.85]
  if h.name eq 'PM_I01402+3147' then limits = [0.72,0.75]
  if h.name eq 'PM_I02164+1335' then limits = [0.90,0.94]
  if h.name eq 'PM_I02358+2013' then limits = [0.88,0.92]
  if h.name eq 'PM_I03095+4543' then limits = [0.88,0.92]
  if h.name eq 'PM_I03181+3815' then limits = [0.88,0.92]
  if h.name eq 'PM_I04290+2155' then limits = [0.88,0.92]
  if h.name eq 'PM_I05421+1229' then limits = [0.88,0.92]
  if h.name eq 'PM_I16254+5418' then limits = [0.855,0.865]
  if h.name eq 'PM_I17095+4340' then limits = [0.88,0.92]
  if h.name eq 'PM_I17198+4142' then limits = [0.85,0.87]
  if h.name eq 'PM_I18453+1851' then limits = [0.88,0.92]
  if h.name eq 'PM_I19220+0702' then limits = [0.88,0.92]
  if h.name eq 'PM_I19396-2645' then limits = [0.86,0.89]
  if h.name eq 'PM_I19539+4424W' then limits = [0.88,0.93]
  if h.name eq 'PM_I20034+2951' then limits = [0.88,0.93]
  if h.name eq 'PM_I20105+0632' then limits = [0.89,0.92]
  if h.name eq 'PM_I20260+5834' then limits = [0.87,0.92]
  if h.name eq 'PM_I22279+5741W' then limits = [0.87,0.92]
  if h.name eq 'PM_I16241+4821' then limits = [0.86,0.88]
  if h.name eq 'PM_I22374+3922' then limits = [0.88,0.92]
  if h.name eq 'PM_I23428+3049' then limits = [0.88,0.92]
  if h.name eq 'PM_I23216+1717' then limits = [0.85,0.88]
  if h.name eq 'PM_I21000+4004E' then limits = [0.88,0.95]
  if h.name eq 'PM_I23318+1956E' then limits = [0.90,0.95]
  if h.name eq 'PM_I22468+4420' then limits = [0.88,0.92]
  if h.name eq 'PM_I11054+4331' then limits = [0.80,0.815]
  if h.name eq 'PM_I07344+6256' then limits = [0.89,0.93]
  if h.name eq 'PM_I02002+1303' then limits = [0.90,0.95]
  if h.name eq 'PM_I17378+1835' then limits = [0.88,0.94]
  if h.name eq 'PM_I17578+4635' then limits = [0.88,0.92]
  if h.name eq 'PM_I16303-1239' then limits = [0.88,0.92]
  if h.name eq 'PM_I01230-1257W' then limits = [0.88,0.92]
  if h.name eq 'LSPM_J0355+5214' then limits = [0.88,0.92]
  if h.name eq 'PM_I05463+0112' then limits = [0.88,0.92]
  if h.name eq 'PM_I15354+6005' then limits = [0.88,0.92]
  if h.name eq 'PM_I18409+3131S' then limits = [0.90,0.92]
  if h.name eq 'PM_I20407+1954' then limits = [0.88,0.92]
  if h.name eq 'PM_I21518+4220E' then limits = [0.86,0.91]
  if h.name eq 'PM_I22160+5439' then limits = [0.88,0.92]
  if h.name eq 'PM_I22524+0954' then limits = [0.90,0.94]
  if h.name eq 'Gl166C' then limits = [0.88,0.92]
  if h.name eq 'PM_I17176+5224' then limits = [0.88,0.92]
  if h.name eq 'PM_I18363+1336S' then limits = [0.73,0.76]
  if h.name eq 'GJ887' then limits = [0.88,0.92]
  if h.name eq 'PM_I02190+2352' then limits = [0.73,0.75]
  if h.name eq 'PM_I14513+1906' then limits = [0.93,0.95]
  if h.name eq 'HD_103095' then limits = [0.88,0.92]
  if h.name eq 'PM_I06548+3316' then limits = [0.88,0.95]
  if h.name eq 'PM_I07446+0333' then limits = [0.88,0.94]
  if h.name eq 'PM_I08161+0118' then limits = [0.88,0.92]
  if h.name eq 'PM_I10564+0700' then limits = [0.75,0.8]
  if h.name eq 'PM_I10430-0912' then limits = [0.88,0.92]
  if h.name eq 'PM_I11054+4331' then limits = [0.88,0.92]
  if h.name eq 'PM_I12388+1141' then limits = [0.88,0.92]
  if h.name eq 'PM_I15194-0743E' then limits = [0.88,0.94]
  if h.name eq 'PM_I15238+1727' then limits = [0.88,0.94]
  if h.name eq 'PM_I23419+4410' then limits = [0.88,0.94]
  if h.name eq 'PM_I09362+3731' then limits = [0.88,0.94]
  if h.name eq 'TWA_9B' then limits = [0.80,0.88]
  if h.name eq 'J04130515145' then limits = [0.8,0.88]
  if h.name eq 'PM_I02234+2244' then limits = [0.88,0.93]
  if h.name eq 'HIP_112312_B' then limits= [0.92,0.97]
  if h.name eq 'GJ2060C' then limits= [0.88,0.95]
  if h.name eq 'J00393579-3816584' then limits=[0.88,0.95]
  if h.name eq 'J03104941-3616471' then limits=[0.88,0.95]
  if h.name eq 'J20474501-3635409' then limits=[0.80,0.88]
  if h.name eq 'J22021626-4210329' then limits=[0.72,0.80]
  if h.name eq '03363144-2619578' then limits = [0.89,0.94]
  if h.name eq 'EPIC_211351816' then limits = [0.77,0.91]
  if h.name eq 'EPIC_211990866' then limits = [0.75,0.92]

  opspec_o = opspec[where(oplambda gt limits[0] and oplambda lt limits[1])]
  operr_o = operr[where(oplambda gt limits[0] and oplambda lt limits[1])]
  oplambda_o = oplambda[where(oplambda gt limits[0] and oplambda lt limits[1])]
  irspec_o = irspec[where(irlambda gt limits[0] and irlambda lt limits[1])]
  irlambda_o = irlambda[where(irlambda gt limits[0] and irlambda lt limits[1])]
  irerr_o = irerr[where(irlambda gt limits[0] and irlambda lt limits[1])]
  
  ;;irspec_o = gaussfold(irlambda_o,irspec_o,snifsfwhm) ; convolve to expected resolution
  irspec_o = interpol(irspec_o,irlambda_o,oplambda_o)
  numbefore = 1.0*n_elements(irerr_o)
  irerr_o = interpol(irerr_o,irlambda_o,oplambda_o)
  numbafter = 1.0*n_elements(irerr_o)
  irerr_o*=sqrt(numbafter/numbefore) ; we gain Signal by root(N) when we bin up data 
  toterr = (irerr_o + operr_o)
  overlap = (irspec_o*(operr_o/toterr) + opspec_o*(irerr_o/toterr))
  overlap_err = 1.0/sqrt(1.0/(irerr_o^2.0) + 1.0/(operr_o^2.0))
  if h.name eq 'PM_I11033+3558' then begin
     overlap = irspec_o
     overlap_err = irerr_o
  endif

  if stis eq 0 then begin
     masterspec = [opspec_b[where(oplambda_b lt min(oplambda))],opspec[where(oplambda lt limits[0])],overlap,irspec[where(irlambda gt limits[1])]]
     masterlambda = [oplambda_b[where(oplambda_b lt min(oplambda))],oplambda[where(oplambda lt limits[0])],oplambda_o,irlambda[where(irlambda gt limits[1])]]
     mastererr = [operr_b[where(oplambda_b lt min(oplambda))],operr[where(oplambda lt limits[0])],overlap_err,irerr[where(irlambda gt limits[1])]]
  endif else begin
     masterspec = [opspec[where(oplambda lt limits[0])],overlap,irspec[where(irlambda gt limits[1])]]
     masterlambda = [oplambda[where(oplambda lt limits[0])],oplambda_o,irlambda[where(irlambda gt limits[1])]]
     mastererr = [operr[where(oplambda lt limits[0])],overlap_err,irerr[where(irlambda gt limits[1])]]
  endelse

  
  l = where(finite(masterspec) eq 1 and finite(masterlambda) eq 1 and finite(mastererr) eq 1 and mastererr gt 0)
  masterspec = masterspec[l]
  masterlambda = masterlambda[l]
  mastererr = mastererr[l]
  plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[0.68,1.1],thick=1,xtitle='Wavelength (Angstroms)'
  oplot,oplambda,opspec,color=cgcolor('green')
  oplot,irlambda,irspec,color=cgcolor('red')


  ;; temporary, making plots for John.
  ;if min(irlambda) lt 0.75 then begin
  ;   !p.multi=[0,1,1]
  ;   set_plot,'PS'
  ;   name = h.cns3
  ;   strreplace,name,' ',''
  ;   strreplace,name,' ',''
  ;   strreplace,name,' ',''
  ;   if strtrim(name,2) eq '' then name = h.name
  ;   device,filename=name+'_overlap_SNF.eps',/encapsul,/color
  ;   plot,oplambda,opspec,xrange=[0.65,1.2],/xstyle,/ystyle,xtitle='Wavelength',ytitle='Flux',xthick=4,ythick=4,charthick=4,charsize=1.5,thick=3
  ;   oplot,irlambda,irspec,color=cgcolor('red'),thick=3
  ;   device,/close
  ;endif
  
  h.fullspec = ptr_new(masterspec)
  h.fulllambda = ptr_new(masterlambda)
  h.fullerr = ptr_new(mastererr)

END


PRO extractdata,h,opspec,operr,oplambda,opspec_b,operr_b,oplambda_b,oprv,irspec,irerr,irlambda,irrv,stis=stis

  stis = 0
  COMPILE_OPT idl2, HIDDEN
  operr = 1
  get_struct_wave_flux, h, oplambda, opspec, var=operr, /restwave
  operr = sqrt(operr)
  operr_b = 1
  get_struct_wave_flux, h, oplambda_b, opspec_b, var=operr_b, /restwave, /blue
  operr_b = sqrt(operr_b)

  if h.name eq 'NLTT19472' then begin
     opspec_b[where(opspec_b lt 0)]*=0
     ;;l = where(finite(opspec_b) eq 1 and finite(operr_b) eq 1 and opspec_b gt 0 and operr_b gt 0)
     ;;opspec_b = opspec_b[l]
     ;;operr_b = operr_b[l]
  endif
  if h.name eq 'PM_I06490+3706' then begin
     opspec_b = abs(opspec_b)
  endif
  if h.name eq 'PM_I10196+1952' then begin ;; invalid variance on blue end
     qq = where(operr lt 0. or finite(operr) eq 0,complement=gg)
     if qq[0] ne -1 then begin
        operr[qq] = median(operr[gg])
     endif
  endif
  ;;if h.name eq 'PM_I10196+1952' then begin
  ;;   operr[where(finite(operr) le 0)] = 0.02*opspec[where(finite(operr) le 0)]
  ;;endif

  ;; if h.cns3 eq 'GJ 905' or h.cns3 eq 'GJ 896A' or h.cns3 eq 'GJ 551' then begin
  ;;    opspec_b+=abs(median(opspec_b[wherE(oplambda_b lt 3600)]))
  ;;    sig = (opspec_b/sqrt(operr_b))
  ;;    l = where(sig lt -3)
  ;;    if l[0] ne -1 then opspec_b[l]+=sqrt(operr_b[l])
  ;;    l = where(sig lt -2)
  ;;    if l[0] ne -1 then opspec_b[l]+=sqrt(operr_b[l])
  ;;    l = where(sig lt -1)
  ;;    if l[0] ne -1 then opspec_b[l]+=sqrt(operr_b[l])
  ;; endif

  if h.cns3 eq 'GJ 570A' then begin
     opspec_b[where(oplambda_b lt 4000)]*=0.90
  endif

  if h.cns3 eq 'GJ 105A' or h.cns3 eq 'GJ 411' then begin
     l = where(finite(operr) eq 0)
     if l[0] ne -1 then operr[l] = 0.005*opspec[l]
  endif

  if h.cns3 eq 'GJ 15B' then begin
     opspec*=(oplambda/median(oplambda))^0.05
  endif
  if h.cns3 eq 'GJ 79' then begin
     opspec*=(oplambda/median(oplambda))^0.2
  endif
  if h.cns3 eq 'GJ 2' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I02050-1736' then begin
     opspec*=(oplambda/median(oplambda))^(-0.2)
  endif
  if h.name eq 'PM_I02123+0334' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  ;;if h.name eq 'PM_I02442+2531' then begin
  ;;   opspec*=(oplambda/median(oplambda))^(0.1)
  ;;endif
  if h.name eq 'PM_I02534+1724' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I05415+5329' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)
  endif
  if h.name eq 'PM_I06171+0507' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)
  endif
  if h.name eq 'PM_I05033-1722' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)
  endif
  if h.name eq 'PM_I05314-0340' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif  
  if h.name eq 'PM_I10196+1952' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I11033+3558' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)     
  endif
  if h.name eq 'PM_I11421+2642' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)     
  endif
  if h.name eq 'PM_I13062+2043' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)     
  endif
  if h.name eq 'PM_I13457+1453' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)     
  endif
  if h.name eq 'PM_I18498-2350' then begin
     ;l = where(oplambda gt 0.85)
     ;opspec[l]*=(oplambda[l]/min(oplambda[l]))^(0.28)     
     opspec*=(oplambda/median(oplambda))^(0.2)     
  endif
  if h.name eq 'PM_I19169+0510' then opspec*=(oplambda/median(oplambda))^(0.1)  
  if h.name eq 'PM_I20533+6209' then opspec*=(oplambda/median(oplambda))^(-0.2) 
  if h.name eq 'PM_I22361-0050' then opspec*=(oplambda/median(oplambda))^(0.1) 
  if h.name eq 'PM_I23492+0224' then opspec*=(oplambda/median(oplambda))^(0.15) 
  if h.name eq 'PM_I06490+3706' then opspec*=(oplambda/median(oplambda))^(0.1) 
  ;;if h.name eq 'PM_I07287-0317' then ;;opspec*=(oplambda/median(oplambda))^(0.25) 
  if h.name eq 'PM_I18051-0301' then opspec*=(oplambda/median(oplambda))^(-0.1) 
  if h.name eq 'PM_I21092-1318' then opspec*=(oplambda/median(oplambda))^(0.1) 
  if h.name eq 'PM_I12194+2822' then opspec*=(oplambda/median(oplambda))^(0.2) 
  ;;if h.name eq 'PM_I11417+4245' then ;opspec*=(oplambda/median(oplambda))^(0.25) 
  if h.name eq 'PM_I08526+2818' then opspec*=(oplambda/median(oplambda))^(-0.15) 
  if h.name eq 'PM_I11311-1457' then opspec*=(oplambda/median(oplambda))^(0.15)
  if h.name eq 'PM_I12507-0046' then opspec*=(oplambda/median(oplambda))^(-0.1)          
  if h.name eq 'PM_I20034+2951' then opspec*=(oplambda/median(oplambda))^(0.1)          
  if h.name eq 'PM_I01571-1014E' then opspec*=(oplambda/median(oplambda))^(-0.2)          
  if h.name eq 'PM_I08105-1348' then opspec*=(oplambda/median(oplambda))^(0.15)          
  if h.name eq 'PM_I10304+5559' then opspec*=(oplambda/median(oplambda))^(0.25)          
  if h.name eq 'PM_I17052-0505' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)          
  endif
  if h.name eq 'PM_I17355+6140' then begin
     opspec*=(oplambda/median(oplambda))^(0.4)          
  endif
  if h.name eq 'PM_I06523-0511' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)          
  endif
  if h.name eq 'PM_I23318+1956W' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)          
  endif
  if h.name eq 'PM_I01186-0052S' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)          
  endif
  if h.name eq 'PM_I02358+2013' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)          
  endif
  if h.name eq 'PM_I03095+4543' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)          
  endif
  if h.name eq 'PM_I04290+2155' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)          
  endif 
  if h.name eq 'PM_I16241+4821' then begin
     opspec*=(oplambda/median(oplambda))^(0.3)          
  endif
  if h.name eq 'PM_I16554-0819' then begin
     opspec*=(oplambda/median(oplambda))^(0.5)
  endif
  if h.name eq 'PM_I17095+4340' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I18453+1851' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I20105+0632' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I21000+4004E' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I02164+1335' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)
  endif
  if h.name eq 'PM_I17303+0532' then begin
     ;;opspec*=(oplambda/median(oplambda))^(-0.15)
  endif
  if h.name eq 'PM_I23428+3049' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)
  endif
  if h.name eq 'PM_I23318+1956E' then begin
     opspec*=(oplambda/median(oplambda))^(0.5)
  endif
  if h.name eq 'PM_I00115+5908' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I01125-1659' then begin
     opspec*=(oplambda/median(oplambda))^(-0.15)
  endif
  if h.name eq 'PM_I01324-2154' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I17578+4635' then begin
     opspec*=(oplambda/median(oplambda))^(0.075)
  endif
  if h.name eq 'PM_I01230-1257W' then begin
     opspec*=(oplambda/median(oplambda))^(0.05)
  endif
  if h.name eq 'PM_I01076+2257E' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'LSPM_J0355+5214' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I05463+0112' then begin
     opspec*=(oplambda/median(oplambda))^(-0.2)
  endif
  if h.name eq 'PM_I11046-0413' then begin
     opspec*=(oplambda/median(oplambda))^(0.23)
  endif
  if h.name eq 'PM_I20407+1954' then begin
     opspec*=(oplambda/median(oplambda))^(0.23)
  endif
  if h.name eq 'PM_I22524+0954' then begin
     opspec*=(oplambda/median(oplambda))^(0.05)
  endif
  if h.name eq 'PM_I17176+5224' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)
  endif
  if h.name eq 'PM_I18363+1336S' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I06548+3316' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I08161+0118' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I08298+2646' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)
  endif
  if h.name eq 'PM_I09447-1812' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I10113+4927' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I10430-0912' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I13299+1022' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I12388+1141' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I16542+1154' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I23182+4617' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I07274+0513' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I10564+0700' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I16303-1239' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I09144+5241' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I13143+1320' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'HIP_106231' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)
  endif
  if h.name eq 'PM_I01036+4051' then opspec*=(oplambda/median(oplambda))^(0.12)     
  if h.name eq 'EPIC_211990866' then opspec*=(oplambda/median(oplambda))^(0.1)     
  ;if h.name eq 'PM_I02234+2244' then begin
  ;   opspec*=(oplambda/median(oplambda))^(0.05)
  ;endif
  if h.stisspec ne ptr_new() then begin
     ;tmp = *h.stisspec
     ;oplambda = tmp.wavelength
     ;opspec = tmp.flux
     ;operr = tmp.staterr
     ;stis = 1
     
     tmp = *h.stisspec
     if max(tmp) ne 0 then begin
        oplambda = tmp[*,0]
        opspec = tmp[*,1]
        operr = tmp[*,2]
        stis = 1
     endif
  endif

  oplambda /= 10000.0
  oplambda_b /= 10000.0

  temp = *h.irtfspec
  if (size(temp))[0] eq 2 then begin
     irlambda = temp[*,0]
     irspec = temp[*,1]
     irerr = temp[*,2]
     irrv = h.irrv
  endif else begin
     irlambda = [temp[*,0,5],temp[*,0,4],temp[*,0,3],temp[*,0,2],temp[*,0,1],temp[*,0,0]]
     irspec = [temp[*,1,5],temp[*,1,4],temp[*,1,3],temp[*,1,2],temp[*,1,1],temp[*,1,0]]
     irerr = [temp[*,2,5],temp[*,2,4],temp[*,2,3],temp[*,2,2],temp[*,2,1],temp[*,2,0]]
     irrv = h.irrv
     print,'Warning '+h.cns3+'/'+h.name+' has not been merged with xmergeorders'
     print,sxpar(*h.IRTFHEADER,'OBJECT')
  endelse
  if h.cns3 eq 'GJ 338A' then begin
     irspec[where(irlambda lt 1.38)]*=0.98
  endif
  if h.cns3 eq 'GJ 526' then begin
     irspec[where(irlambda lt 1.38)]*=0.98
  endif
  if h.name eq 'PM_I02171+3526' then begin
     irspec[where(irlambda gt 1.8)]*=0.98
  endif
  if h.name eq 'PM_I22361-0050' then begin
     irspec[where(irlambda gt 1.8)]*=1.03
  endif
  if h.name eq 'PM_I04376+5253' then begin
     irspec[where(irlambda gt 1.8)]*=0.93
  endif
  if h.name eq 'PM_I02534+1724' then begin
     irspec[where(irlambda gt 1.8)]*=0.91
  endif
  if h.name eq 'PM_I11046-0413' then begin
     irspec[where(irlambda gt 1.8)]*=1.05
  endif
  if h.name eq 'GJ631' then begin
     irspec[where(irlambda gt 1.8)]*=0.92
  endif
  ;;if h.name eq 'PM_I23245+5751S' then begin
  ;;   irspec[where(irlambda gt 1.8)]*=0.98
  ;;endif
  if h.name eq 'PM_I03095+4543' then begin
     irspec[where(irlambda gt 1.8)]*=1.02
  endif
  if h.name eq 'PM_I23318+1956W' then begin
     l = where(irlambda lt 1.4)
     irspec[l]*=0.95
     ;;irspec[l]*=(irlambda[l]/median(irlambda[l]))^(0.2)
  endif
  if h.name eq 'PM_I01186-0052S' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I02164+1335' then begin
     irspec*=(irlambda/median(irlambda))^(0.25)          
  endif
  if h.name eq 'PM_I16254+5418' then begin
     irspec*=(irlambda/median(irlambda))^(-0.3)          
  endif
  if h.name eq 'PM_I17095+4340' then begin
     irspec*=(irlambda/median(irlambda))^(-0.05)          
  endif
  if h.name eq 'PM_I17115+3826' then begin
     irspec*=(irlambda/median(irlambda))^(-0.075)          
  endif
  if h.name eq 'PM_I18165+4533' then begin
     irspec*=(irlambda/median(irlambda))^(-0.05)          
  endif
  if h.name eq 'PM_I18353+4544' then begin
     irspec*=(irlambda/median(irlambda))^(-0.2)          
  endif
  if h.name eq 'PM_I18453+1851' then begin
     irspec*=(irlambda/median(irlambda))^(0.15)          
  endif
  if h.name eq 'PM_I19216+2052' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I16241+4821' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  ;;if h.name eq 'PM_I16554-0819' then begin
  ;;   irspec*=(irlambda/median(irlambda))^(0.1)          
  ;;endif
  if h.name eq 'PM_I22374+3922' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I23428+3049' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I23099+1425W' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  ;;if h.name eq 'PM_I03181+3815' then begin
  ;;   irspec*=(irlambda/median(irlambda))^(-0.1)          
  ;;endif
  if h.name eq 'PM_I21000+4004E' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)          
  endif
  if h.name eq 'PM_I00115+5908' then begin
     irspec*=(irlambda/median(irlambda))^(-0.075)          
  endif
  if h.name eq 'PM_I22468+4420' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)          
  endif
  if h.name eq 'PM_I18046+1354' then begin
     irspec*=(irlambda/median(irlambda))^(0.07)          
  endif
  if h.name eq 'PM_I11033+3558' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)          
  endif
  if h.name eq 'PM_I11054+4331' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I02442+2531' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I01125-1659' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)          
  endif
  if h.name eq 'PM_I06140+5140' then begin
     irspec*=(irlambda/median(irlambda))^(-0.15)
  endif
  if h.name eq 'PM_I02171+3526' then begin
     irspec*=(irlambda/median(irlambda))^(0.08)
  endif
  if h.name eq 'PM_I07344+6256' then begin
     irspec*=(irlambda/median(irlambda))^(-0.08)
  endif
  if h.name eq 'PM_I23245+5751S' then begin
     irspec*=(irlambda/median(irlambda))^(-0.08)
  endif
  if h.name eq 'PM_I01324-2154' then begin
     irspec*=(irlambda/median(irlambda))^(0.04)
  endif
  if h.name eq 'PM_I17578+4635' then begin
     irspec*=(irlambda/median(irlambda))^(-0.01)
  endif
  if h.name eq 'PM_I19072+2052' then begin
     irspec*=(irlambda/median(irlambda))^(-0.01)
  endif
  if h.name eq 'PM_I01230-1257W' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I03047+6144' then begin
     irspec*=(irlambda/median(irlambda))^(-0.04)
  endif
  if h.name eq 'LSPM_J0355+5214' then begin
     irspec*=(irlambda/median(irlambda))^(-0.06)
  endif
  if h.name eq 'PM_I06461+3233' then begin
     irspec*=(irlambda/median(irlambda))^(-0.15)
  endif
  if h.name eq 'PM_I15354+6005' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I16148+6038' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq 'PM_I18409+3131S' then begin
     irspec*=(irlambda/median(irlambda))^(-0.09)
  endif
  if h.name eq 'PM_I20407+1954' then begin
     irspec*=(irlambda/median(irlambda))^(-0.16)
  endif
  if h.name eq 'PM_I22160+5439' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq 'PM_I22524+0954' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'Gl166C' then begin
     irspec*=(irlambda/median(irlambda))^(-0.15)
  endif
  if h.name eq 'PM_I18419+3149' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'PM_I18363+1336S' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'LSPM_J2106+3844S' then begin
     irspec*=(irlambda/median(irlambda))^(-0.075)
  endif
  if h.name eq 'GJ631' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq 'PM_I14513+1906' then begin
     irspec*=(irlambda/median(irlambda))^(-0.3)
  endif
  if h.name eq 'PM_I03526+1701' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'PM_I05421+1229' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'PM_I08298+2646' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'PM_I09095+3249N' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I09437-1747' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I10430-0912' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I10289+0050' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I12388+1141' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I18427+5937N' then begin
     irspec*=(irlambda/median(irlambda))^(-0.05)
  endif
  if h.name eq 'PM_I23182+4617' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'PM_I09362+3731' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq 'PM_I01036+4051' then begin
     irspec*=(irlambda/median(irlambda))^(-0.05)
  endif
  if h.name eq '02405344-1724188_' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq '22583200+1014589_' then begin
     irspec*=(irlambda/median(irlambda))^(0.2)
  endif
  if h.name eq 'J20474501-3635409' then begin
     irspec*=(irlambda/median(irlambda))^(0.2)
  endif
  if h.name eq 'HD_217379' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq 'EPIC_211822797' then begin
     irspec*=(irlambda/median(irlambda))^(-0.18)
  endif
  if h.name eq 'EPIC_211913977' then begin
     irspec*=(irlambda/median(irlambda))^(0.08)
  endif
  ;;if h.name eq 'PM_I17176+5224' then begin
  ;;   irspec*=(irlambda/median(irlambda))^(-0.15)
  ;;endif
  ;; uSpeX has an artifact at the far red end, lets set the error
  ;; there to infinity
  q = where(irlambda lt 2.54)
  irlambda = irlambda[q]
  irspec = irspec[q]
  irerr = irerr[q]

  order = sort(irlambda)
  irlambda = irlambda[order]
  irspec = irspec[order]
  irerr = irerr[order]
  locs = where(finite(irspec) eq 1 and finite(irerr) eq 1)
  irlambda = irlambda[locs]
  irspec = irspec[locs]
  irerr = irerr[locs]

END


FUNCTION myRJL,X,P
  COMPILE_OPT idl2, HIDDEN
  t = p[0]/x^p[1]               ; + p[1]/x^4.0
  RETURN, t
END

FUNCTION myWein,X,P
  COMPILE_OPT idl2, HIDDEN
  t = (p[0]/x^5.0)*exp(p[1]/x)
  RETURN, t
END


function TR_fit3,teff,metal,p
  rad = 0
  for i = 0,n_elements(p)-2 do rad+= p[i]*teff^(1.*i)
  rad = (1.+p[i]*metal)*rad
  ;rad+=p[i]*metal*rad
  return,rad
end


;this procedure takes a spectrum and splits out SDSS magnitudes the
;input is a list of spectra 
;The syntax is simply- specmags,filelist -  the output is plotted,
;                      written to the screan and  is written to a
;                      files named specmag.dat

; NAME: SPECMAGS

                                ;PURPOSE:
;   Convert spectra to SDSS AB mags     
;
; CALLING SEQUENCE:
;      SPECMAGS, filelist
;       
; INPUTS:
;      filelist - list of spectra (right now the code expects sdss spectra)
;      
;
; OUTPUTS:
;   file named 'specmag.dat' is written in the directory you run
;   this.  You can change that if you like.   Results are also written
;   to screen and a r-i,g-r plot is given
;      
;      


function response,ilambda, ivac_pass, lambda
;this interpolates over the SDSS filter curves and matches the number
;of elements in the data

  ;;response=SPLINE(ilambda, ivac_pass, lambda)
  response = interpol(ivac_pass,ilambda,lambda,/spline)

  return,response

end


function filter, lambda, flux, glambda,gcurve,rlambda,rcurve,ilambda,icurve,zlambda,zcurve

  ;;airtovac,lambda
  fl_g = interpol(flux,lambda,glambda)
  fl_r = interpol(flux,lambda,rlambda)
  fl_i = interpol(flux,lambda,ilambda)
  fl_z = interpol(flux,lambda,zlambda)

  ergtojans = 1.d23
  speedoflight = 2.99792458d18
  gflux = gcurve*fl_g*ergtojans*glambda^2./speedoflight
  rflux = rcurve*fl_r*ergtojans*rlambda^2./speedoflight
  iflux = icurve*fl_i*ergtojans*ilambda^2./speedoflight
  zflux = zcurve*fl_z*ergtojans*zlambda^2./speedoflight
  ;;gflux=double(fl_g*gflux*(glambda^2/2.998d18)*1.d23)
  ;;iflux=double(ires*flux(iind)*(lambda(iind)^2/2.998d18)*1.d23)
  ;;zflux=double(zres*flux(zind)*(lambda(zind)^2/2.998d18)*1.d23)


;integrate using trapezoidal rule to get the total flux and then
;divide by the "effective" filter width to get the flux density in
;Janskys
  ;;umag=abs(tsum(3.d18/lambda(uind),uflux)/(2.998d18*tsum(lambda(uind),ures)/3551.^2))
  ;;gmag=abs(tsum(3.d18/glambda,gflux)/(2.998d18*tsum(glambda,gcurve)/4686.^2))
  ;;rmag=abs(tsum(3.d18/lambda(rind),rflux)/(2.998d18*tsum(lambda(rind),rres)/6165.^2))
  ;;imag=abs(tsum(3.d18/lambda(iind),iflux)/(2.998d18*tsum(lambda(iind),ires)/7481.^2))
  ;;zmag=abs(tsum(3.d18/lambda(zind),zflux)/(2.998d18*tsum(lambda(zind),zres)/8931.^2))

  ;;gmag=abs(tsum(3.d18/glambda,gflux)/(2.998d18*tsum(glambda,gcurve)/4719.^2))
  ;;rmag=abs(tsum(3.d18/rlambda,rflux)/(2.998d18*tsum(rlambda,rcurve)/6185.^2))
  ;;imag=abs(tsum(3.d18/ilambda,iflux)/(2.998d18*tsum(ilambda,icurve)/7500.^2))
  ;;zmag=abs(tsum(3.d18/zlambda,zflux)/(2.998d18*tsum(zlambda,zcurve)/8961.^2))
  gmag=integral(glambda,gflux)/integral(glambda,gcurve)
  rmag=integral(rlambda,rflux)/integral(rlambda,rcurve)
  imag=integral(ilambda,iflux)/integral(ilambda,icurve)
  zmag=integral(zlambda,zflux)/integral(zlambda,zcurve)


  ;;rmag=integral(lambda[rind],rflux)/integral(lambda[rind],rres)
  ;;imag=integral(lambda[iind],iflux)/integral(lambda[iind],ires)
  ;;zmag=integral(lambda[zind],zflux)/integral(lambda[zind],zres)

  ;;color_plots
  
  fluxes=[gmag,rmag,imag,zmag]

  
  return,fluxes

end

function filter_old, lambda, flux, glambda,gcurve,rlambda,rcurve,ilambda,icurve,zlambda,zcurve

;read in the filter curves.  For a star or small source you will want
;to change the format to f='f,f'  this is because for extended sources
;the filter curves are slightly different.

;readcol,'u.dat',ulambda,ucurve,f='f,x,f',/silent

;these indices define where the data overlap the curves. For sdss
;spectra, only the g, r, and i filters completely.  If you are using
;spectra that span different wavelenght ranges you will need to alter
;them.  I have commented out u and z indices in the ideal case that
;you span the entire wavelength range of sdss filters

;uind=where(lambda ge min(ulambda) and lambda le max(ulambda))
;uind=where(lambda le max(ulambda))
  gind=where(lambda ge min(glambda) and lambda le max(glambda))
  rind=where(lambda ge min(rlambda) and lambda le max(rlambda))
  iind=where(lambda ge min(ilambda) and lambda le max(ilambda))
  zind=where(lambda ge min(zlambda) and lambda le max(zlambda))
;zind=where(lambda ge min(zlambda)and lambda le max(zlambda))

;these are testing where the filters overlap the data (comment out in
;the ideal case of having full wavelength coverage)
;uresind=where(ulambda ge min(lambda))
;;zresind=where(zlambda le max(lambda))

;calculate the response for each band
;ures=response(ulambda(uresind),ucurve(uresind),lambda(uind))

  gres=response(glambda,gcurve,lambda(gind))
  rres=response(rlambda,rcurve,lambda(rind))
  ires=response(ilambda,icurve,lambda(iind))
  zres=response(zlambda,zcurve,lambda(zind))
;;zres=response(zlambda(zresind),zcurve(zresind),lambda(zind))

;convolve the response with the flux and convert to Janksys

;uflux=double(ures*flux(uind)*(lambda(uind)^2/2.998d18)*1.d23)
  gflux=double(gres*flux(gind)*(lambda(gind)^2/2.998d18)*1.d23)
  rflux=double(rres*flux(rind)*(lambda(rind)^2/2.998d18)*1.d23)
  iflux=double(ires*flux(iind)*(lambda(iind)^2/2.998d18)*1.d23)
  zflux=double(zres*flux(zind)*(lambda(zind)^2/2.998d18)*1.d23)


;integrate using trapezoidal rule to get the total flux and then
;divide by the "effective" filter width to get the flux density in
;Janskys

;umag=abs(tsum(3.d18/lambda(uind),uflux)/(2.998d18*tsum(lambda(uind),ures)/3551.^2))
  ;;gmag=abs(tsum(3.d18/lambda(gind),gflux)/(2.998d18*tsum(lambda(gind),gres)/4686.^2))
  ;;rmag=abs(tsum(3.d18/lambda(rind),rflux)/(2.998d18*tsum(lambda(rind),rres)/6165.^2))
  ;;imag=abs(tsum(3.d18/lambda(iind),iflux)/(2.998d18*tsum(lambda(iind),ires)/7481.^2))
  ;;zmag=abs(tsum(3.d18/lambda(zind),zflux)/(2.998d18*tsum(lambda(zind),zres)/8931.^2))

  gmag=integral(lambda[gind],gflux)/integral(lambda[gind],gres)
  rmag=integral(lambda[rind],rflux)/integral(lambda[rind],rres)
  imag=integral(lambda[iind],iflux)/integral(lambda[iind],ires)
  zmag=integral(lambda[zind],zflux)/integral(lambda[zind],zres)

  fluxes=[gmag,rmag,imag,zmag]

  return,fluxes

end


function jansk2mag, janskys
;sdss softening parameters
;;b_u = 1.4D-10
  b_g = 0.9D-10
  b_r = 1.2D-10
  b_i = 1.8D-10
  b_z = 7.4D-10

;calculate asinh AB mags

;;umag = -1.*(2.5/alog(10))*[asinh((janskys[0]/3631.)/(2*b_u))+alog(b_u)]
  gmag = -1.*(2.5/alog(10))*[asinh((janskys[0]/3631.)/(2*b_g))+alog(b_g)]
  rmag = -1.*(2.5/alog(10))*[asinh((janskys[1]/3631.)/(2*b_r))+alog(b_r)]
  imag = -1.*(2.5/alog(10))*[asinh((janskys[2]/3631.)/(2*b_i))+alog(b_i)]
  zmag = -1.*(2.5/alog(10))*[asinh((janskys[3]/3631.)/(2*b_z))+alog(b_z)]

  mags=[gmag,rmag,imag,zmag]

  return, mags

end
