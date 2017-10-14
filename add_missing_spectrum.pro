;; this program will add in the missing regions of the spectrum (<3200\AA, >25000\AA, the gap at 18000\AA and the telluric line at 1.4, maybe the telluric line at 1.1?

PRO wrapper

  COMPILE_OPT IDL2, HIDDEN
  restore,'~/Dropbox/Structures/rdtargets.dat'
  r = rdtargets[where(strmid(rdtargets.comment,0,7) eq 'Tabetha' and rdtargets.irtfsnr gt 60 and rdtargets.uh22snr gt 60)]
  for i = 0,n_elements(r)-1 do begin
     print,r[i].name,' ',r[i].cns3
     l = where(rdtargets.name eq r[i].name)
     rd = rdtargets[l]
     add_missing_spectrum,rd
     rdtargets[l] = rd
  endfor
  stop
  get_juldate,juldate
  juldate = strtrim(string(juldate-2455000,format="(D8.3)"),2)
  spawn,'pwd',path
  if strpos(path,'amann') ne -1 then spawn,'mv ~/Dropbox/Structures/RDtargets.dat /Volumes/Vali/ArchivedStructures/RDtargets_'+juldate+'.dat'
  if strpos(path,'andrewmann') ne -1 then spawn,'mv ~/Dropbox/Structures/RDtargets.dat ~/Desktop/ArchivedStructures/RDtargets_'+juldate+'.dat'
  save,filename='~/Dropbox/Structures/RDtargets.dat',rdtargets

END

PRO single,name

  COMPILE_OPT IDL2, HIDDEN
  restore,'~/Dropbox/Structures/rdtargets.dat'
  l = where(rdtargets.cns3 eq name)
  if l[0] eq -1 then stop
  r = rdtargets[l]
  add_missing_spectrum,r
  rdtargets[l] = r
  stop
  get_juldate,juldate
  juldate = strtrim(string(juldate-2455000,format="(D8.3)"),2)
  spawn,'pwd',path
  if strpos(path,'amann') ne -1 then spawn,'mv ~/Dropbox/Structures/RDtargets.dat /Volumes/Vali/ArchivedStructures/RDtargets_'+juldate+'.dat'
  if strpos(path,'andrewmann') ne -1 then spawn,'mv ~/Dropbox/Structures/RDtargets.dat ~/Desktop/ArchivedStructures/RDtargets_'+juldate+'.dat'
  save,filename='~/Dropbox/Structures/RDtargets.dat',rdtargets
  
END


PRO add_missing_spectrum,r,add=add,compvalues=compvalues,dup=dup,hot=hot,conroy=conroy,plot=plot

  if n_elements(plot) eq 0 then plot = 1
  if n_elements(conroy) eq 0 then conroy = 0
  if n_elements(add) eq 0 then add = 1
  if n_elements(hot) eq 0 then hot = 0
  if add ge 1 then begin
     COMPILE_OPT IDL2, HIDDEN
     ;;models = mrdfits('~/Dropbox/Kepler/Teff_code/Models_Filler_Jun2013.fits',1) ;; this is a special grid that has a wider wavelength range
     ;;models = mrdfits('~/Dropbox/Radii/Models_ASPLUND_Aug2013_filler.fits',1,/silent)
     models = mrdfits('~/Dropbox/Radii/Models_CIFIST_Aug2013_filler.fits',1,/silent)
     if n_elements(dup) gt 0 then models = mrdfits('~/Dropbox/Radii/Models_CIFIST_Dupuy.fits',1,/silent)
     if hot eq 1 then models = mrdfits('~/Dropbox/Radii/Models_hot_Jun2016.fits',1,/silent)
     
     mh = r.irtffeh
     if mh lt -1.0 then metalrange=[-1.6,-0.9]
     if mh ge -1.0 and mh lt -0.5 then metalrange=[-1.1,-0.4]
     if mh ge -0.5 and mh lt 0.0 then metalrange=[-0.6,0.1]
     if mh ge 0.0 and mh lt 0.3 then metalrange=[-0.1,0.4]
     if mh ge 0.3 then metalrange=[0.2,0.6]
     
     flux = *r.fullspec
     wave = *r.fulllambda*1d4
     var = (*r.fullerr)^2.0

     max_lambda = 1d5           ;3d4
     if n_elements(dup) eq 1 then max_lambda = 3d5
     l = where(wave gt 10000)
     ;;l = where(wave lt 10000)
     if r.cns3 eq 'GJ 876' then l = where(wave lt 8000)
     if r.cns3 eq 'GJ 649' then l = where(wave gt 10000)
     if r.name eq 'PM_I13143+1320' then metalrange=[-0.1,0.6]
     if r.name eq 'EPIC_205117205' or r.name eq 'EPIC_205046529' then metalrange = [-0.1,0.1]
     if r.name eq 'EPIC_205117205' then l = where(wave gt 12000)
     if r.name eq '2M1728+39' then l = where(wave gt 13000)
     if r.name eq '2M2206-20' then l = where(wave gt 8000 and wave lt 15000)
     if r.name eq '2M1404-31' then l = where(wave gt 8000 and wave lt 15000)
     if r.name eq '2M1534-29' then l = where(wave gt 19700 and wave lt 24000)
     if r.name eq '2M2206-20' then l = where(wave gt 8000 and wave lt 15000)
     if r.name eq 'SD0423-04' then l = where(wave gt 18000)
     if r.name eq '2M0920+35' then l = where(wave gt 20000)
     if r.name eq '2M2140+16' then l = where(wave gt 0 and wave lt 14000)
     fwhm = 7000./1300.
     if n_elements(dup) gt 0 then fwhm = median(wave-shift(wave,1))*2.0
     run_j9_on_models_2,wave[l],flux[l],var[l],models,compvalues, metalrange=metalrange, savespec=savespec,savemetal=savemetal,saveteff=saveteff,savelogg=savelogg,saveweight=saveweight,dup=dup,fwhm=fwhm,plot=0,hot=hot
     ;;print,'Fit Teff: ',string(compvalues[6],format="(I4)")+' K'
     
     flux = *r.fullspec
     modelspectra = models.spectrum
     modelheader = models.header
     lambda_m = sxpar(modelheader,'LAMBDA_0') + findgen(sxpar(modelheader,'NLAMBDA'))*sxpar(modelheader,'D_LAMBDA')
     teff = models.teff
     logg = abs(models.logg)
     metal = models.metal
     model1 = modelspectra[(where(teff eq saveteff[0] and abs(logg) eq abs(savelogg[0]) and metal eq savemetal[0]))[0],*]
     model2 = modelspectra[(where(teff eq saveteff[1] and abs(logg) eq abs(savelogg[1]) and metal eq savemetal[1]))[0],*]
     model3 = modelspectra[(where(teff eq saveteff[2] and abs(logg) eq abs(savelogg[2]) and metal eq savemetal[2]))[0],*]
     mspec = saveweight[0]*model1/mean(model1) + (saveweight[1]-saveweight[0])*model2/mean(model2) + (1.-saveweight[1])*model3/mean(model3)
     if n_elements(dup) gt 0 then mspec = gaussfold(lambda_m,mspec,fwhm) else  mspec = gaussfold(lambda_m,mspec,fwhm)
     
     modelspec = mspec*(median(flux[where(wave gt 1.7d4 and wave lt 1.75d4)])/median(mspec[where(lambda_m gt 1.7d4 and lambda_m lt 1.75d4)]))
     dlambda = abs(median(wave-shift(wave,1)))
     if n_elements(dup) gt 0 then dlambda = 20.
     if r.name eq 'SD0423-04' or r.name eq '2M1728+39' or r.name eq '2M2252-17' then dlambda = 50.
     intermodelwave = generatearray(2000,max_lambda-50,(max_lambda)/dlambda)
     modelspec = interpol(modelspec,lambda_m,intermodelwave)
     w = wave/1d4

     if r.name eq 'EPIC_205117205' or r.name eq 'EPIC_205046529' then begin
        modelspec[where(intermodelwave gt 1.34d4 and intermodelwave le 1.45d4)]*=0.9
     endif
     ;;if r.name eq 'EPIC_211901114' then begin   
     ;;   modelspec[where(intermodelwave gt 1.7d4 and intermodelwave le 2.1d4)]*=1.03
     ;;endif

     if conroy eq 0 then begin
        pl1 = 1.34
        pl2 = 1.45
        pl3 = 1.77
        pl4 = 2.1
     endif else begin
        pl1 = 1.35
        pl2 = 1.43
        pl3 = 1.79
        pl4 = 2.1
     endelse
     
     if add eq 1 then begin
        fancyflux = [$          ;modelspec[where(intermodelwave lt min(wave))],$
                    flux[where(w le pl1)],modelspec[where(intermodelwave gt pl1*1d4 and intermodelwave le pl2*1d4)],$
                    flux[where(w gt pl2 and w le pl3)],modelspec[where(intermodelwave gt pl3*1d4 and intermodelwave le pl4*1d4)],$
                    flux[where(w gt pl4)],modelspec[where(intermodelwave gt max(wave))]]
        fancywave = [$          ;intermodelwave[where(intermodelwave lt min(wave))],$
                    wave[where(w le pl1)],intermodelwave[where(intermodelwave gt pl1*1d4 and intermodelwave le pl2*1d4)],$
                    wave[where(w gt pl2 and w le pl3)],intermodelwave[where(intermodelwave gt pl3*1d4 and intermodelwave le pl4*1d4)],$
                    wave[where(w gt 2.1)],intermodelwave[where(intermodelwave gt max(wave))]]
        fancyerr = [$           ;dblarr(n_elements(where(intermodelwave lt min(wave)))),$
                   var[where(w le pl1)],dblarr(n_elements(where(intermodelwave gt pl1*1d4 and intermodelwave le pl2*1d4))),$
                   var[where(w gt pl2 and w le pl3)],dblarr(n_elements(where(intermodelwave gt pl3*1d4 and intermodelwave le pl4*1d4))),$
                   var[where(w gt pl4)],dblarr(n_elements(where(intermodelwave gt max(wave))))]
        if n_elements(dup) gt 0 then begin
           redcut3 = 2.0
           bluecut3 = 1.8
           llm = where(intermodelwave lt min(Wave)+100d0 and intermodelwave gt min(wave))
           lls = where(wave lt min(wave)+100d0)
           bcoeff = median(flux[lls])/median(modelspec[llm])
           llm = where(intermodelwave gt max(Wave)-100d0 and intermodelwave lt max(wave))
           lls = where(wave gt max(wave)-100d0)
           ;if r.name eq '2M0700+31' then begin
           ;   tmp = findgen(n_elements(where(intermodelwave gt 3d4)))
           ;   modelspec[where(intermodelwave gt 3d4)]*=(tmp/median(tMp))^0.01
           ;endif
           rcoeff = median(flux[lls])/median(modelspec[llm])
           if r.name eq '2M0700+31' then rcoeff*=1.13
           if r.name eq 'SD0423-04' then rcoeff*=1.12
           if r.name eq '2M1728+39' then rcoeff*=1.05
           if r.name eq '2M0920+35' then rcoeff*=1.1
           if r.name eq '2M2140+16' then begin
              bcoeff*=1.15
              rcoeff*=1.08
           endif
           if r.name eq '2M2206-20' then bcoeff*=0.96
           if r.name eq 'GL417B' then rcoeff*=1.1
           if r.name eq 'SD1052+44' then rcoeff*=1.15
           ;;if r.name eq '2M1534-29' then rcoeff*=1.2
           fancyflux = [modelspec[where(intermodelwave lt min(wave))]*bcoeff,$
                       flux[where(w le pl1)],modelspec[where(intermodelwave gt pl1*1d4 and intermodelwave le pl2*1d4)],$
                       flux[where(w gt pl2 and w le bluecut3)],modelspec[where(intermodelwave gt bluecut3*1d4 and intermodelwave le redcut3*1d4)],$
                       flux[where(w gt redcut3)],modelspec[where(intermodelwave gt max(wave))]*rcoeff]
           fancywave = [intermodelwave[where(intermodelwave lt min(wave))],$
                       wave[where(w le pl1)],intermodelwave[where(intermodelwave gt pl1*1d4 and intermodelwave le pl2*1d4)],$
                       wave[where(w gt pl2 and w le bluecut3)],intermodelwave[where(intermodelwave gt bluecut3*1d4 and intermodelwave le redcut3*1d4)],$
                       wave[where(w gt redcut3)],intermodelwave[where(intermodelwave gt max(wave))]]
           fancyerr = [dblarr(n_elements(where(intermodelwave lt min(wave)))),$
                      var[where(w le pl1)],dblarr(n_elements(where(intermodelwave gt pl1*1d4 and intermodelwave le pl2*1d4))),$
                      var[where(w gt pl2 and w le bluecut3)],dblarr(n_elements(where(intermodelwave gt bluecut3*1d4 and intermodelwave le redcut3*1d4))),$
                      var[where(w gt redcut3)],dblarr(n_elements(where(intermodelwave gt max(wave))))]
        endif
           
     endif

     if plot eq 1 then begin
        set_plot,'x'
        !p.multi = [0,1,1,0,0]
     endif
     if r.name eq 'EPIC_205117205' or r.name eq 'EPIC_205046529' then begin
        models2 = mrdfits('~/Dropbox/Radii/Models_CIFIST_Apr2016_long.fits',1,/silent)
        modelspectra2 = models2.spectrum
        modelheader2 = models2.header
        lambda_m2 = sxpar(modelheader2,'LAMBDA_0') + findgen(sxpar(modelheader2,'NLAMBDA'))*sxpar(modelheader2,'D_LAMBDA')
        teff2 = models2.teff
        logg2 = abs(models2.logg)
        metal2 = models2.metal
        newmodel = modelspectra2[where(teff2 eq 3500),*]
        newmodel = newmodel[*]
        qq1 = where(fancywave gt 9d4 and fancywave lt 10d4)
        qq2 = where(lambda_m2 gt 9d4 and lambda_m2 lt 10d4)
        newmodel/=median(newmodel[qq2])/median(fancyflux[qq1])
        
        if plot eq 1 then begin
           plot,w,flux,xrange=[2,30],/xlog,/ylog,yrange=[1d-20,1d-14],/xstyle
           oplot,intermodelwave/1d4,modelspec,color=cgcolor('red')
           past = where(lambda_m2 gt max(fancywave))
           oplot,lambda_m2[past]/1d4,newmodel[past],color=cgcolor('teal')
        endif
        
        fancyflux = [fancyflux,newmodel[past]]
        fancywave = [fancywave,lambda_m2[past]]
        fancyerr = [fancyerr,dblarr(n_elements(past))]
     endif
     
     
     if add eq 2 then begin
        fancyflux = [flux,modelspec[where(intermodelwave gt max(wave))]]
        fancywave = [wave,intermodelwave[where(intermodelwave gt max(wave))]]
        fancyerr = [var,dblarr(n_elements(where(intermodelwave gt max(wave))))]
     endif
     if plot eq 1 then begin
        !p.multi=[0,2,2,0,0]
        plot,fancywave,fancyflux,/xstyle,/ystyle,yrange=[min(fancyflux[where(fancyflux gt 0)]),max(fancyflux)],/xlog,/ylog
        oplot,fancywave[where(fancyerr eq 0.0)],fancyflux[where(fancyerr eq 0.0)],color=cgcolor('red')
        plot,fancywave,fancyflux,/xstyle,/ystyle,xrange=[1.3d4,1.6d4]
        oplot,fancywave[where(fancyerr eq 0.0)],fancyflux[where(fancyerr eq 0.0)],color=cgcolor('red')

        plot,fancywave,fancyflux,/xstyle,/ystyle,xrange=[1.6d4,2.2d4]
        oplot,fancywave[where(fancyerr eq 0.0)],fancyflux[where(fancyerr eq 0.0)],color=cgcolor('red')

        plot,fancywave,fancyflux,/xstyle,/ystyle,xrange=[2.0d4,2.7d4]
        oplot,fancywave[where(fancyerr eq 0.0)],fancyflux[where(fancyerr eq 0.0)],color=cgcolor('red')
        ;;print,r.newteff,compvalues[6],compvalues[7]

        ll1 = where(fancyerr eq 0.0 and fancywave gt 1.3d4 and fancywave lt 1.5d4)
        ll2 = where(fancyerr eq 0.0 and fancywave gt 1.7d4 and fancywave lt 2.2d4)
     endif
     fancydata = [[fancywave],[fancyflux],[fancyerr]]
     ;;stop
     
  endif else begin
     flux = *r.fullspec
     wave = *r.fulllambda*1d4
     var = (*r.fullerr)^2.0
     fancydata = [[wave],[flux],[var]]
  endelse
  r.fspec = ptr_new(fancydata)

END

