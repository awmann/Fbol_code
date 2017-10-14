PRO gcal_plotter,r,phot_system,offset,offset_thresh,name,lambda,spec,sp_err,lams,phfluxes,phfluxes_err,fwhms,FLUXES,FLUXES_err,add=add,inset=inset,long=long,$
                 oc=oc,e_corr=e_corr,corr=corr,dup=dup,printname=printname ;; 0 (default) o-c in terms of sigmas, 1 = in terms of magnitudes

  err = sp_err
  if n_elements(oc) eq 0 then oc = 0
  if n_elementS(printname) eq 0 then printname = 0
  if n_elements(long) eq 0 then long = 0
  if r.name eq 'USco1610' then long = 1
  if n_elements(add) eq 0 then add = 0
  if n_elements(inset) eq 0 then inset = 0
  charthick = 3
  charsize = 1.25
  xthick = 5
  ythick = 5
  thick = 3.0
  errthick = 3.0
  plotsym,0,/fill
  xrange = [0.34,4.0]            ;[0.25,0.6];
  xrange_blue = [0.25,0.6]
  xrange_red = [2.0,30]
  ;;if long eq 1 then xrange_red = [2.0,100]
  if long eq -1 then xrange = [0.36,2.4]
  if inset eq 1 then thick = 4
  cgloadct,0
  !p.font=0
;;if add eq 1 then begin

  angstrom = cgsymbol("Angstrom")
  angstrom ='A'
  sigma = textoidl('\sigma')
  ;;angstrom = '!6!sA!r!u!9 %!6!n'
  ;;angstrom = STRING("305B)
  
  errsym = 8
  errthick=xthick
  l = where(offset lt offset_thresh,COMPLEMENT=m)
  factor = round(alog10(median(spec[where(lambda gt 0.4 and lambda lt 2.4)])))
  set_plot,'PS'
  photcolor = cgcolor('red')
  !p.multi=[0,1,2]
  xmargin = !x.margin
  ymargin = !y.margin
  !x.margin = [9,2]
  !y.margin = [-5,1]

  downsampler = findgen(n_elements(lambda));findgen(n_elements(lambda)/4)*4
  
  if long eq -1 then device,filename='Phot_fits/'+name+'_'+string(add,format="(I1)")+'.eps',/encapsul,/color,xsize=26,ysize=13 else device,filename='Phot_fits/'+name+'_'+string(add,format="(I1)")+'.eps',/encapsul,/color
  plot,lambda[downsampler],spec[downsampler]/(10.^factor),xrange=xrange,/xstyle,/ystyle,xtickn=strarr(10)+' ',charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,thick=thick,ytitle='Flux (10!U'+strtrim(string(factor,format="(I3)"))+'!N erg cm!U-2!N s!U-1!N '+angstrom+'!U-1!N)'
  sharpcorners,thick=xthick,color=cgcolor('black')
  modelcolor = cgcolor('dark grey')
  if add eq 1 then begin
     q = where(err gt median(err)*0.9995 and err lt median(err)*1.0005 and lambda gt 1.31 and lambda lt 1.45)
     if n_Elements(q) gt 10 then begin
        q = q[where((q-shift(q,1)) le 1)]
        oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     endif
     q = where(err gt median(err)*0.9995 and err lt median(err)*1.0005 and lambda gt 1.75 and lambda lt 2.1)
     if n_Elements(q) gt 10 then begin
        q = q[where((q-shift(q,1)) le 1)]
        oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     endif
     q = where(err gt median(err)*0.9995 and err lt median(err)*1.0005 and lambda lt 1.0)
     if n_Elements(q) gt 10 then begin
        q = q[where((q-shift(q,1)) le 1)]
        oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     endif
     
  endif
  if add ge 1 then begin
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 2.35)
     if n_Elements(q) gt 10 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
  endif
  photcolor = cgcolor('red')

  
  oploterror,lams[l],phfluxes[l]/(10.^factor),fwhms[l],phfluxes_err[l]/(10.^factor),color=photcolor,psym=3,errthick=errthick,errcolor=photcolor 
  oplot,lams[l],fluxes[l]/(10.^factor),color=cgcolor('blue'),psym=8,symsize=1.0

  if printname eq 1 then begin
     name = r.name
     strreplace,name,'_',' '
     xyouts,0.7,0.73,name,/normal,charsize=charsize,charthick=charthick
  endif
  if r.name eq 'HD_189733' then begin
     xyouts,3.2,0.5,'HD 189733',charsize=charsize,charthick=charthick
  endif
  if r.name eq 'HD_209458' then begin
     xyouts,3.0,0.5,'HD 209458',charsize=charsize,charthick=charthick
  endif
  xx_tmp = 13000
  yy_tmp = 9000
  if r.cns3 eq 'GJ 411' then begin
     xyouts,xx_tmp,yy_tmp,'Gl 411',charsize=charsize,charthick=charthick,/device
  endif
  if r.cns3 eq 'GJ 699' then begin
     xyouts,xx_tmp,yy_tmp,'Gl 699',charsize=charsize,charthick=charthick,/device
  endif
  if r.cns3 eq 'GJ 880' then begin
     xyouts,xx_tmp,yy_tmp,'Gl 880',charsize=charsize,charthick=charthick,/device
  endif
  if r.cns3 eq 'GJ 876' then begin
     xyouts,xx_tmp,yy_tmp,'Gl 876',charsize=charsize,charthick=charthick,/device
  endif

  sharpcorners,thick=xthick,color=cgcolor('black')

;;xyouts,2.5,median(spec[where(lambda gt 1 and lambda lt 3)]/(10.^factor)),string(rchisq_cover,format='(D5.2)'),charsize=charsize,alignment=0.5
  if add ge 1 then begin
     legend,[' ','              ','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')],psym=[0,0,8,8],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')]
     legend,['Data','BT-SETTL Model',' '],color=[cgcolor('black'),cgcolor('dark grey'),photcolor],psym=[0,0,8],/top,/right,charsize=charsize/1.25,box=0,thick=[10,10,0],textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor]
  endif else begin
     legend,['Spectrum','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('red'),cgcolor('blue')],psym=[0,8,8],thick=[10,0,0],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('red'),cgcolor('blue')]
  endelse

  if inset eq 1 then begin
     plot, lambda, spec/(10.^factor), xrange=[0.38,0.49], /ystyle,/xstyle, /noeras,ytickname=strarr(10)+' ',thick=thick,xthick=thick,ythick=thick,color=cgcolor('black'),xtitle='Wavelength (!9m!3m)',charsize=0.9,position=[0.3,0.42,0.6,0.6] ;position=[0.62,0.52,0.94,0.77],
     sharpcorners,thick=xthick,color=cgcolor('black')
  endif

  !y.margin=[4,5]
  sharpcorners,thick=xthick,color=cgcolor('black')
  if oc eq 0 then begin ;; sigmas
     yrange = [-3.5,3.5]        ;[-0.2,0.2]
     sigma = textoidl('\sigma')
     plot,[0],[0],xrange=xrange,yrange=yrange,charsize=charsize,charthick=charthick,xthick=5,ythick=5,xtitle='Wavelength (!9m!3m)',/xstyle,/ystyle,ytickn=['-3','0','3'],yticks=2,ytickv=[-3,0,3],ytitle='Residual ('+sigma+')',xticklen=0.08,yminor=5
     sharpcorners,thick=thick,color=cgcolor('black')
;;,ytickn=['-0.2','0.0','0.2'],yticks=2,ytickv=[-0.2,0,0.2]
     oplot,lams[l],(phfluxes[l]-(fluxes[l]))/sqrt(fluxes_err[l]^2.0+phfluxes_err[l]^2.0),psym=errsym
  endif else begin
     magdiff = -2.5*alog10(phfluxes/fluxes)
     nmonte = 1d5
     magdiff_err = dblarr(n_elements(magdiff))
     for jjj = 0,n_elements(phfluxes)-1 do begin
        tmp1 = phfluxes[jjj]+phfluxes_err[jjj]*randomn(seed,nmonte)
        tmp2 = fluxes[jjj]+fluxes_err[jjj]*randomn(seed,nmonte)
        tmp3 = -2.5*alog10(tmp1/tmp2)
        magdiff_err[jjj] = robust_sigma(tmp3)
     endfor
     q = where(lams gt xrange[0] and lams lt xrange[1])
     min = min([magdiff[q]-magdiff_err[q]])
     max = max([magdiff[q]+magdiff_Err[q]])
     yrange = [-1d0*max(abs([min,max]))-0.005,max(abs([min,max]))+0.005]
     ploterror,lams[l],magdiff[l],lams[l]*0d0,magdiff_err[l],psym=errsym,errthick=thick,xrange=xrange,/xstyle,/ystyle,xtitle='Wavelength (!9m!3m)',charsize=charsize,charthick=charthick,ytitle='Residual (mag)',yrange=yrange,xticklen=0.08,yminor=5,xthick=xthick,ythick=ythick
  endelse
  oplot,[-10,10],[0,0],linestyle=2,thick=4,color=cgcolor('black')
  sharpcorners,thick=xthick,color=cgcolor('black')

  device,/close

  if n_elements(dup) eq 0 then begin
     xrange_trans = [0.7,1.1]
     !x.margin = [9,2]
     !y.margin = [-5,1]
     device,filename='Phot_fits/'+name+'_trans.eps',/encapsul,/color
     photcolor = cgcolor('red')
     plot,lambda,spec/(10.^factor),xrange=xrange_trans,/xstyle,/ystyle,xtitle=' ',ytitle='Flux (10!U'+strtrim(string(factor,format="(I3)"))+'!N erg cm!U-2!N s!U-1!N '+angstrom+'!U-1!N)',xtickn=strarr(10)+' ',charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,thick=thick
     sharpcorners,thick=xthick,color=cgcolor('black')
     modelcolor = cgcolor('dark grey')
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 1.3 and lambda lt 1.5)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 1.7 and lambda lt 2.15)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 2.5)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     photcolor = cgcolor('red')
     oploterror,lams[l],phfluxes[l]/(10.^factor),fwhms[l],phfluxes_err[l]/(10.^factor),color=photcolor,psym=3,errthick=errthick,errcolor=photcolor 
     oplot,lams[l],fluxes[l]/(10.^factor),color=cgcolor('blue'),psym=8,symsize=0.5
     if add eq 1 then begin
        legend,[' ','              ','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')],psym=[0,0,8,8],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')]
        legend,['Data','BT-SETTL Model',' '],color=[cgcolor('black'),cgcolor('dark grey'),photcolor],psym=[0,0,8],/top,/right,charsize=charsize/1.25,box=0,thick=[10,10,0],textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor]
     endif else begin
        legend,['Spectrum','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('red'),cgcolor('blue')],psym=[0,8,8],thick=[10,0,0],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('red'),cgcolor('blue')]
     endelse

     !y.margin=[4,5]
     yrange = [-3,3]            ;[-0.2,0.2]
     plot,[0],[0],xrange=xrange_trans,yrange=yrange,charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,xtitle='Wavelength (!9m!3m)',/xstyle,/ystyle,$
          ytickn=['-3','0','3'],yticks=2,ytickv=[-3,0,3]
     oplot,lams[l],(phfluxes[l]-(fluxes[l]))/sqrt(fluxes_err[l]^2.0+phfluxes_err[l]^2.0),psym=errsym
     oplot,[-10,10],[0,0],linestyle=2,thick=3,color=cgcolor('black')
     device,/close

     xrange_trans2 = [0.4,0.6]
     !x.margin = [9,2]
     !y.margin = [-5,1]
     device,filename='Phot_fits/'+name+'_trans2.eps',/encapsul,/color
     photcolor = cgcolor('red')
     plot,lambda,spec/(10.^factor),xrange=xrange_trans2,/xstyle,/ystyle,xtitle=' ',ytitle='Flux (10!U'+strtrim(string(factor,format="(I3)"))+'!N erg cm!U-2!N s!U-1!N '+angstrom+'!U-1!N)',xtickn=strarr(10)+' ',charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,thick=thick
     sharpcorners,thick=xthick,color=cgcolor('black')
     modelcolor = cgcolor('dark grey')
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 1.3 and lambda lt 1.5)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 1.7 and lambda lt 2.15)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 2.5)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     photcolor = cgcolor('red')
     oploterror,lams[l],phfluxes[l]/(10.^factor),fwhms[l],phfluxes_err[l]/(10.^factor),color=photcolor,psym=3,errthick=errthick,errcolor=photcolor 
     oplot,lams[l],fluxes[l]/(10.^factor),color=cgcolor('blue'),psym=8,symsize=0.5
     if add eq 1 then begin
        legend,[' ','              ','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')],psym=[0,0,8,8],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')]
        legend,['Data','BT-SETTL Model',' '],color=[cgcolor('black'),cgcolor('dark grey'),photcolor],psym=[0,0,8],/top,/right,charsize=charsize/1.25,box=0,thick=[10,10,0],textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor]
     endif else begin
        legend,['Spectrum','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('red'),cgcolor('blue')],psym=[0,8,8],thick=[10,0,0],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('red'),cgcolor('blue')]
     endelse

     !y.margin=[4,5]
     yrange = [-3,3]            ;[-0.2,0.2]
     plot,[0],[0],xrange=xrange_trans2,yrange=yrange,charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,xtitle='Wavelength (!9m!3m)',/xstyle,/ystyle,$
          ytickn=['-3','0','3'],yticks=2,ytickv=[-3,0,3]
     oplot,lams[l],(phfluxes[l]-(fluxes[l]))/sqrt(fluxes_err[l]^2.0+phfluxes_err[l]^2.0),psym=errsym
     oplot,[-10,10],[0,0],linestyle=2,thick=3,color=cgcolor('black')
     device,/close


     !y.margin = [-5,1]
     photcolor = cgcolor('red')
     device,filename='Phot_fits/'+name+'_blue.eps',/encapsul,/color
     plot,lambda,spec/(10.^factor),xrange=xrange_blue,/xstyle,/ystyle,xtitle=' ',ytitle='Flux (10!U'+strtrim(string(factor,format="(I3)"))+'!N erg cm!U-2!N s!U-1!N '+angstrom+'!U-1!N)',xtickn=strarr(10)+' ',charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,thick=thick
     sharpcorners,thick=xthick,color=cgcolor('black')
     modelcolor = cgcolor('dark grey')
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 1.3 and lambda lt 1.5)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 1.7 and lambda lt 2.15)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 2.5)
     if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
     photcolor = cgcolor('red')
     oploterror,lams[l],phfluxes[l]/(10.^factor),fwhms[l],phfluxes_err[l]/(10.^factor),color=photcolor,psym=3,errthick=errthick
     oplot,lams[l],fluxes[l]/(10.^factor),color=cgcolor('blue'),psym=8,symsize=0.5
     if add eq 1 then begin
        legend,[' ','              ','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')],psym=[0,0,8,8],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')]
        legend,['Data','BT-SETTL Model',' '],color=[cgcolor('black'),cgcolor('dark grey'),photcolor],psym=[0,0,8],/top,/right,charsize=charsize/1.25,box=0,thick=[10,10,0],textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor]
     endif else begin
        legend,['Spectrum','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('red'),cgcolor('blue')],psym=[0,8,8],thick=[10,0,0],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('red'),cgcolor('blue')]
     endelse

     !y.margin=[4,5]
     yrange = [-3,3]            ;[-0.2,0.2]
     plot,[0],[0],xrange=xrange_blue,yrange=yrange,charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,xtitle='Wavelength (!9m!3m)',/xstyle,/ystyle,$
          ytickn=['-3','0','3'],yticks=2,ytickv=[-3,0,3]
;;,ytickn=['-0.2','0.0','0.2'],yticks=2,ytickv=[-0.2,0,0.2]
     oplot,lams[l],(phfluxes[l]-(fluxes[l]))/sqrt(fluxes_err[l]^2.0+phfluxes_err[l]^2.0),psym=errsym
;;for jj = 0,n_elements(l)-1 do begin
;;   tmp = (phfluxes[l[jj]]-(fluxes[l[jj]]))/(fluxes[l[jj]])
;;   oplot,[lams[l[jj]],lams[l[jj]]]+0.01*randomn(seed),[0,tmp],color=cgcolor('dark green'),thick=3
;;endfor
     oplot,[-10,10],[0,0],linestyle=2,thick=3,color=cgcolor('black')
     device,/close
  endif

  !y.margin = [-5,1]
;;factor-=2
  factor = 0.
  oldl = l
  l = where(offset lt offset_thresh or phot_system eq 'Wise' or phot_system eq 'Spitzer') ; we should include wise for this plot
  cgloadct,0
  photcolor = cgcolor('red')
  device,filename='Phot_fits/'+name+'_IR.eps',/encapsul,/color
  ;; sample down the spectrum
  tmp1 = findgen(N_elements(spec))
  tmp2 = where(tmp1 mod 3 eq 1)
  plot,lambda[tmp2],spec[tmp2]/(10.^factor),xrange=xrange_red,/xstyle,/ystyle,xtitle=' ',ytitle='Flux (erg cm!U-2!N s!U-1!N '$
       +angstrom+'!U-1!N)',xtickn=strarr(10)+' ',charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,/xlog,/ylog,thick=thick
  sharpcorners,thick=xthick,color=cgcolor('black')
  modelcolor = cgcolor('dark grey')
  q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 1.3 and lambda lt 1.5)
  if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
  q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 1.7 and lambda lt 2.15)
  if q[0] ne -1 then oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
  q = where(err gt median(err)*0.995 and err lt median(err)*1.005 and lambda gt 2.4)
  if q[0] ne -1 then  oplot,lambda[q],(spec[q])/(10.^factor),color=modelcolor,thick=thick
  oploterror,lams[l],phfluxes[l]/(10.^factor),fwhms[l],phfluxes_err[l],color=photcolor,psym=3,errthick=errthick,errcolor=photcolor
  oplot,lams[l],fluxes[l]/(10.^factor),color=cgcolor('blue'),psym=8,symsize=0.5
  if add eq 1 then begin
     legend,[' ','              ','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')],psym=[0,0,8,8],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor,cgcolor('blue')]
     legend,['Data','BT-SETTL Model',' '],color=[cgcolor('black'),cgcolor('dark grey'),photcolor],psym=[0,0,8],/top,/right,charsize=charsize/1.25,box=0,thick=[10,10,0],textcolor=[cgcolor('black'),cgcolor('dark grey'),photcolor]
  endif else begin
     legend,['Spectrum','Photometry','Synthetic Photometry'],color=[cgcolor('black'),cgcolor('red'),cgcolor('blue')],psym=[0,8,8],thick=[10,0,0],/top,/right,charsize=charsize/1.25,box=0,textcolor=[cgcolor('black'),cgcolor('red'),cgcolor('blue')]
  endelse

  !y.margin=[4,5]
  if r.name eq 'EPIC_205117205' then begin
     yrange = [-7,7] 
     plot,[0],[0],xrange=xrange_red,yrange=yrange,charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,xtitle='Wavelength (!9m!3m)',/xstyle,/ystyle,ytickn=['-6','-3','0','3','6'],yticks=4,ytickv=[-6,-3,0,3,6],/xlog,ytitle='Residual ('+sigma+')',xtickn=strtrim([2,3,4,5,6,7,8,9,10,20,30],2),xtickv=[2,3,4,5,6,7,8,9,10,20,30],xticks=10,xticklen=0.06
;;,ytickn=['-0.2','0.0','0.2'],yticks=2,ytickv=[-0.2,0,0.2]
  endif else begin
     plot,[0],[0],xrange=xrange_red,yrange=yrange,charsize=charsize,charthick=charthick,xthick=xthick,ythick=ythick,xtitle='Wavelength (!9m!3m)',/xstyle,/ystyle,ytickn=['-3','0','3'],yticks=2,ytickv=[-3,0,3],/xlog,ytitle='Residual ('+sigma+')',xtickn=strtrim([2,3,4,5,6,7,8,9,10,20,30],2),xtickv=[2,3,4,5,6,7,8,9,10,20,30],xticks=10,xticklen=0.06
  endelse

  if n_elements(e_corr) eq 0 then begin
     corr = dblarr(n_elements(fluxes))+1d0
     e_corr = corr*0.01
  endif
  err = (phfluxes[l]-(fluxes[l]))/sqrt(fluxes_err[l]^2.0+phfluxes_err[l]^2.0+(fluxes[l]*(e_corr[l]/corr[l]))^2.)
  oplot,lams[l],err,psym=errsym
;;for jj = 0,n_elements(l)-1 do begin
;;   tmp = (phfluxes[l[jj]]-(fluxes[l[jj]]))/(fluxes[l[jj]])
;;   oplot,[lams[l[jj]],lams[l[jj]]]+0.01*randomn(seed),[0,tmp],color=cgcolor('dark green'),thick=3
;;endfor
  oplot,[1,1d5],[0,0],linestyle=2,thick=3,color=cgcolor('black')
  device,/close
  
  l = oldl
  !x.margin = xmargin
  !y.margin = ymargin
  set_plot,'x'

END
