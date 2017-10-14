
;; makes the fancy file of photometry
PRO create_photometry_file,r,redo,rem
  
  COMPILE_OPT idl2, HIDDEN
  ;; get the photometry
  name = r.cns3
  strreplace,name,' ',''
  strreplace,name,' ',''
  strreplace,name,' ',''
  strreplace,name,' ',''
  strreplace,name,' ',''
  strreplace,name,' ',''
  if strtrim(name,2) eq '' then name = r.name
  file = 'photometry/'+name+'.txt'
  if file_test(file) eq 0 or redo eq 1 then begin
     if r.othername ne '' and r.othername ne 'dummy' and r.name ne 'PM_I23438+3235' and r.name ne 'PM_I02441+4913W' and r.name ne 'PM_I05463+0112' and r.name ne 'PM_I10008+3155' and r.name ne 'PM_I16148+6038' and r.name ne 'PM_I22467+1210' then begin 
        starter = 'python2.7 gcpd3/GCPD3.py --target '
        ;; execute the python script
        if rem ne '' then adder = '--rem '+rem else adder = ''
        ;;print,starter+r.othername+' --system UBV '+adder
        spawn,starter+r.othername+' --system UBV '+adder+' > '+file         ; added
        spawn,starter+r.othername+' --system UBVE '+adder+' >> '+file       ; added
        spawn,starter+r.othername+' --system DDO '+adder+' >> '+file        ; added
        spawn,starter+r.othername+' --system Geneva '+adder+' >> '+file     ; added
        spawn,starter+r.othername+' --system UBVRI '+adder+' >> '+file      ; added
        spawn,starter+r.othername+' --system RI_Cousins '+adder+' >> '+file ; added
        spawn,starter+r.othername+' --system RI_Eggen '+adder+' >> '+file   ; added 
        spawn,starter+r.othername+' --system uvby '+adder+' >> '+file       ; added
        spawn,starter+r.othername+' --system WBVR '+adder+' >> '+file       ; added 
        spawn,starter+r.othername+' --system Vilnius '+adder+' >> '+file    ; added
        spawn,starter+r.othername+' --system Straizys '+adder+' >> '+file   ; added
        spawn,starter+r.othername+' --system IJHKLMN '+adder+' >> '+file    ; added
        ;;stop
        ;;spawn,starter+r.othername+' --system Walraven '+adder+' >> '+file  ; added UNABLE TO FIND ZERO-POINT
        ;;spawn,starter+r.othername+' --system RI_Kron >> '+file  ; added UNABLE TO FIND ZERO-POINT
        if strtrim(r.cns3,2) eq 'GJ 176' then begin                                     ;; this one has two entries in GCPD...
           nm = 'Gliese\ 176'
           spawn,starter+nm+' --system UBV >> '+file        ; added
           spawn,starter+nm+' --system UBVE >> '+file       ; added
           spawn,starter+nm+' --system DDO >> '+file        ; added
           spawn,starter+nm+' --system Geneva >> '+file     ; added
           spawn,starter+nm+' --system UBVRI >> '+file      ; added
           spawn,starter+nm+' --system IJHKLMN >> '+file    ; added
           spawn,starter+nm+' --system RI_Cousins >> '+file ; added
           spawn,starter+nm+' --system RI_Eggen >> '+file   ; added THIS KINDA SUCKS
           spawn,starter+nm+' --system ubvy >> '+file       ; added
           spawn,starter+nm+' --system WBVR >> '+file       ; added 
           spawn,starter+nm+' --system Vilnius >> '+file    ; added
           spawn,starter+nm+' --system Straizys >> '+file   ; added
        endif
        if strtrim(r.cns3,2) eq 'GJ 15A' then begin                                     ;; this one has two entries in GCPD...
           adder = '--rem AV'
           spawn,starter+r.othername+' --system UBV '+adder+' >> '+file        ; added
           spawn,starter+r.othername+' --system UBVE '+adder+' >> '+file       ; added
           spawn,starter+r.othername+' --system DDO '+adder+' >> '+file        ; added
           spawn,starter+r.othername+' --system Geneva '+adder+' >> '+file     ; added
           spawn,starter+r.othername+' --system UBVRI '+adder+' >> '+file      ; added
           spawn,starter+r.othername+' --system RI_Cousins '+adder+' >> '+file ; added
           spawn,starter+r.othername+' --system RI_Eggen '+adder+' >> '+file   ; added THIS KINDA SUCKS
           spawn,starter+r.othername+' --system uvby '+adder+' >> '+file       ; added
           spawn,starter+r.othername+' --system WBVR '+adder+' >> '+file       ; added 
           spawn,starter+r.othername+' --system Vilnius '+adder+' >> '+file    ; added
           spawn,starter+r.othername+' --system Straizys '+adder+' >> '+file   ; added
           spawn,starter+r.othername+' --system IJHKLMN '+adder+' >> '+file    ; added
        endif
     endif else begin
        close,25
        openw,25,file
        printf,25,'# no appropriate name for star to run GCPD3.py'
        close,25
     endelse

     close,25
     openw,25,file,/append
     if r.cns3 ne 'GJ 702B' then begin ;; grabs A, B is not in 2mass or tycho
        if r.cns3 ne 'GJ 338A' then begin
           info = queryvizier('2MASS-PSC',[r.ra,r.dec],0.25,/allcolumns,/silent)
           if isarray(info) then begin
              if n_elements(info) ge 2 then begin
                 gcirc,2,r.ra,r.dec,info.raj2000,info.dej2000,dis3
                 info = info[where(dis3 eq min(dis3))]
                 info = info[0]
              endif
              jm = info.jmag
              jm_err = info.e_jmag
              hm = info.hmag
              hm_err = info.e_hmag
              km = info.kmag
              km_err = info.e_kmag
              if jm ge 0 and finite(jm) eq 1 and jm_err gt 0 and finite(jm_err) eq 1 then begin
                 str = 'M '+r.othername+' 2Mass J '+string(jm,format="(D6.3)")+' '+string(jm_err,format="(D5.3)")+'  # 2Mass J'
                 printf,25,str
              endif
              if hm ge 0 and finite(hm) eq 1 and hm_err gt 0 and finite(hm_err) eq 1 then begin
                 str = 'M '+r.othername+' 2Mass H '+string(hm,format="(D6.3)")+' '+string(Hm_err,format="(D5.3)")+'  # 2Mass H'
                 printf,25,str
              endif
              if jm ge 0 and finite(Km) eq 1 and Km_err gt 0 and finite(Km_err) eq 1 then begin
                 str = 'M '+r.othername+' 2Mass K '+string(Km,format="(D6.3)")+' '+string(Km_err,format="(D5.3)")+'  # 2Mass K'
                 printf,25,str
              endif
           endif
        endif
        if r.cns3 ne 'GJ 725B' and r.cns3 ne 'GJ 725A' and r.name ne 'PM_I05024-2115' and r.name ne 'PM_I01125-1659' and r.name ne 'PM_I13062+2043' and r.name ne 'PM_I11417+4245' and r.name ne 'PM_I20525-1658' and r.name ne 'PM_I05033-1722' and r.name ne 'PM_I01076+2257E' and r.name ne 'PM_I01571-1014E' and r.name ne 'PM_I02441+4913W' and r.name ne 'PM_I01186-0052S' and r.name ne 'PM_I22467+1210' then begin ;; these get confused
           info = QueryVizier('I/259/TYC2',[r.ra,r.dec],0.5,/allcolumns,/s)
           if isarray(info) and r.name ne 'PM_I23318+1956E' then begin
              if n_elements(info) ge 2 then begin
                 gcirc,2,info.RA_ICRS,info.DE_ICRS,r.ra,r.dec,dist
                 info = info[where(dist eq min(dist))]
                 info = info[0]           
              endif
              bt = info.btmag
              bt_err = info.e_btmag
              vt = info.vtmag
              vt_err = info.e_vtmag
              if bt ge 0 and finite(bt) eq 1 and bt_err gt 0 and finite(bt_err) eq 1 then begin
                 str = 'M '+r.othername+' Tycho Bt '+string(bt,format="(D6.3)")+' '+string(bt_err,format="(D5.3)")+'  # Tycho Bt'
                 printf,25,str
              endif
              if vt ge 0 and finite(vt) eq 1 and vt_err gt 0 and finite(vt_err) eq 1 then begin
                 str = 'M '+r.othername+' Tycho Vt '+string(vt,format="(D6.3)")+' '+string(vt_err,format="(D5.3)")+'  # Tycho Vt'
                 printf,25,str
              endif
           endif else begin
              info = QueryVizier('I/259/suppl_1',[r.ra,r.dec],0.5,/allcolumns,/silent)
              if isarray(info) then begin
                 if n_elements(info) ge 2 then begin
                    gcirc,2,info.RA_ICRS,info.DE_ICRS,r.ra,r.dec,dist
                    info = info[where(dist eq min(dist))]
                    info = info[0]           
                 endif
                 bt = info.btmag
                 bt_err = info.e_btmag
                 vt = info.vtmag
                 vt_err = info.e_vtmag
                 if bt ge 0 and finite(bt) eq 1 and bt_err gt 0 and finite(bt_err) eq 1 then begin
                    str = 'M '+r.othername+' Tycho Bt '+string(bt,format="(D6.3)")+' '+string(bt_err,format="(D5.3)")+'  # Tycho Bt'
                    printf,25,str
                 endif
                 if hm ge 0 and finite(vt) eq 1 and vt_err gt 0 and finite(vt_err) eq 1 then begin
                    str = 'M '+r.othername+' Tycho Vt '+string(vt,format="(D6.3)")+' '+string(vt_err,format="(D5.3)")+'  # Tycho Vt'
                    printf,25,str
                 endif
              endif
           endelse
        endif
     endif
     info = queryvizier('I/322A/out',[r.ra,r.dec],0.25,/allcolumns,/silent)
     if isarray(info) then begin
        if n_elements(info) gt 1 then begin
           gcirc,2,r.ra,r.dec,info.RAJ2000,info.DEJ2000,dist
           info = info[where(dist eq min(dist))]
           info = info[0]           
        endif
        v = info.vmag
        v_err = info.E_VMAG/100.
        b = info.bmag
        b_err = info.e_bmag/100.
        if v_err lt 0.01 then v_err = 0.05
        if b_err lt 0.01 then b_err = 0.05
        if v gt 10. or b gt 10. then printf,25,'# APASS photometry:'
        if v gt 10. and finite(v) eq 1 and finite(v_err) eq 1 then begin
           str = 'M '+r.othername+' Johnson B '+string(B,format="(D6.3)")+' '+string(v_err,format="(D5.3)")+'  # Johnson B'
           printf,25,str
        endif
        if b gt 10. and finite(b) eq 1 and finite(b_err) eq 1 then begin
           str = 'M '+r.othername+' Johnson V '+string(V,format="(D6.3)")+' '+string(b_err,format="(D5.3)")+'  # Johnson V'
           printf,25,str
        endif
     endif
     
     info = queryvizier('I/311/HIP2',[r.ra,r.dec],1.0,/allcolumns,/silent)
     if isarray(info) and r.name ne 'PM_I02555+2652' and r.name ne 'PM_I18427+5937S' and r.name ne 'PM_I01076+2257E' and r.name ne 'PM_I01571-1014E' and r.name ne 'PM_I02441+4913W' and r.name ne 'PM_I06523-0511' and r.name ne 'PM_I22467+1210' then begin
        if n_elements(info) gt 1 then begin
           gcirc,2,r.ra,r.dec,info.RARAD,info.derad,dist
           info = info[where(dist eq min(dist))]
           info = info[0]           
        endif
        hp = info.hpmag
        hp_err = info.e_hpmag ;;*10.0
        if hp ge 0 and finite(hp) eq 1 and hp_err gt 0 and finite(hp_err) eq 1 then begin
           if hp gt r.k then begin
              str = 'M '+r.othername+' Hipparcos Hp '+string(Hp,format="(D6.3)")+' '+string(Hp_err,format="(D5.3)")+'  # Hipparcos Hp'
              printf,25,str
           endif
        endif
     endif
     if r.cns3 ne 'GJ 570A' and r.name ne 'PM_I01076+2257E' then begin ;; it grabs 570B...
        info = queryvizier('J/MNRAS/403/1949/ubvri',[r.ra,r.dec],0.75,/allcolumns,/silent)
        if isarray(info) then begin
           if n_elements(info) gt 1 then begin ;; This needs to be checked and updated
              gcirc,2,r.ra,r.dec,info._ra,info._de,dist
              info = info[where(dist eq min(dist))]
              info = info[0]           
           endif
           printf,25,'# Koen UBVRI photometry'
           v = info.vmag
           b = info.vmag + info.b_v
           u = b + info.U_B
           rmag = v - info.V_RC
           imag = v - info.V_IC
           printf,25,'# Koen Photometry:'
           err_v = info.e_vmag ;; for now assume we are dominated by error in V (they don't provide errors on other photometry)
           str = 'M '+r.othername+' Johnson U '+string(u,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson U'
           printf,25,str
           str = 'M '+r.othername+' Johnson B '+string(b,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson B'
           printf,25,str
           str = 'M '+r.othername+' Johnson V '+string(v,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson V'
           printf,25,str
           str = 'M '+r.othername+' Cousins R '+string(rmag,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Cousins R'
           printf,25,str
           str = 'M '+r.othername+' Cousins I '+string(imag,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Cousins I'
           printf,25,str
           if info.j eq 'J' then begin
              koen = mrdfits('Koen_jhkl.fit',1,/silent)
              ;;print,r.hip,' has JHKL in Koen'
              printf,25,'# Koen JHKL photometry'
              l = where(koen.hip eq r.hip)
              if l[0] eq -1 then stop
              str = 'M '+r.othername+' Johnson J '+string(koen[l].jmag,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson J'
              printf,25,str
              str = 'M '+r.othername+' Johnson H '+string(koen[l].hmag,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson H'
              printf,25,str
              str = 'M '+r.othername+' Johnson K '+string(koen[l].kmag,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson K'
              printf,25,str
              str = 'M '+r.othername+' Johnson L '+string(koen[l].lmag,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson L'
              printf,25,str
           endif
        endif
     endif
     if r.cns3 ne 'GJ 702B' and r.cns3 ne 'GJ 212' and r.name ne 'PM_I08526+2818' and r.name ne 'PM_I10196+1952' and r.name ne 'PM_I19169+0510' and r.name ne 'PM_I02441+4913W' and r.name ne 'PM_I20407+1954' and r.name ne 'PM_I22467+1210' then begin
        info = queryvizier('II/237/colors',[r.ra,r.dec],3.0,/allcolumns,/silent)
        if isarray(info) then begin
           printf,25,'# Ducati 2002, 12-color Johnson'
           if r.cns3 eq 'GJ 820B' then info = info[0]
           if n_elements(info) gt 1 then begin ;; This needs to be checked and updated
              ratmp = dblarr(n_elements(info))
              dectmp = ratmp
              for iii = 0,n_elements(ratmp)-1 do begin
                 ratmp[iii] = am_racnv(info[iii]._RA_ICRS)
                 dectmp[iii] = am_deccnv(info[iii]._de_ICRS)
              endfor
              gcirc,2,r.ra,r.dec,ratmp,dectmp,dist
              info = info[where(dist eq min(dist))]
              info = info[0]           
           endif
           v = info.vmag
           b = info.b_v+info.vmag
           u = info.u_v+info.vmag
           rmag = info.r_v+info.vmag
           imag = info.i_v+info.vmag
           j = info.j_v+info.vmag
           h = info.h_v+info.vmag
           k = info.k_v+info.vmag
           L = info.l_v+info.vmag
           M = info.m_v+info.vmag
           N = info.n_v+info.vmag
           err_v = 0.05 ;; who the f knows... not reported!
           str = 'M '+r.othername+' Johnson U '+string(u,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson U'
           if u gt 0 and finite(u) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson B '+string(b,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson B'
           if b gt 0 and finite(b) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson R '+string(rmag,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson R'
           if rmag gt 0 and finite(rmag) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson I '+string(imag,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson I'
           if imag gt 0 and finite(imag) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson J '+string(j,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson J'
           if j gt 0 and finite(j) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson H '+string(h,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson H'
           if h gt 0 and finite(h) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson K '+string(k,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson K'
           if k gt 0 and finite(k) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson L '+string(l,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson L'
           if l gt 0 and finite(l) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson M '+string(m,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson M'
           if m gt 0 and finite(m) eq 1 then printf,25,str
           str = 'M '+r.othername+' Johnson N '+string(n,format="(D6.3)")+' '+string(err_v,format="(D5.3)")+'  # Johnson N'
           if n gt 0 and finite(n) eq 1 then printf,25,str
        endif
     endif


     info = queryvizier('II/328/allwise',[r.ra,r.dec],0.1)
     if isarray(info) then begin
        if n_elements(info) gt 1 then begin
           gcirc,2,r.ra,r.dec,info.raj2000,info.dej2000,dist
           info = info[where(dist eq min(dist))]
           info = info[0]
        endif
        l = 0
        w1 = info.w1mag
        w1_err = info.e_w1mag
        w2 = info.w2mag
        w2_err = info.e_w2mag
        w3 = info.W3mag
        w3_err = info.e_W3Mag
        w4 = info.W4Mag
        w4_err = info.e_w4mag

        str = 'M '+r.othername+' Wise W1 '+string(w1,format="(D7.4)")+' '+string(w1_err,format="(D7.4)")+'  # Wise W1'
        if finite(w1_err) eq 1 and finite(w1) eq 1 and w1 gt -50 and w1_err gt -50 then printf,25,str
        str = 'M '+r.othername+' Wise W2 '+string(w2,format="(D7.4)")+' '+string(w2_err,format="(D7.4)")+'  # Wise W2'
        if finite(w2_err) eq 1 and finite(w2) eq 1 and w2 gt -50 and w2_err gt -50 then printf,25,str
        str = 'M '+r.othername+' Wise W3 '+string(w3,format="(D7.4)")+' '+string(w3_err,format="(D7.4)")+'  # Wise W3'
        if finite(w3_err) eq 1 and finite(w3) eq 1 and w3 gt -50 and w3_err gt -50 then printf,25,str
        str = 'M '+r.othername+' Wise W4 '+string(w4,format="(D7.4)")+' '+string(w4_err,format="(D7.4)")+'  # Wise W4'
        if finite(w4_err) eq 1 and finite(w4) eq 1 and w4 gt -50 and w4_err gt -50 then printf,25,str
     endif
     
     ;; WISE
     ;; readcol,'wise_allwise.wise_allwise_p3as_psd12149.tbl',c1,c2,c3,c4,c50,name,c5,ra,dec,w1,w1_err,w2,w2_err,w3,w3_err,w4,w4_err,w1_sat,w2_sat,w3_sat,w4_sat,cc_flag,ext_flag,ph_qual,delimiter=' ',format='d,d,d,d,d,a,a,d,d,d,d,d,d,d,d,d,d,d,d,d,d,a,a,a',/silent
     ;; gcirc,2,ra,dec,r.ra,r.dec,dist
     ;; l = where(Dist lt 50 and (w1 lt r.k+3.))
     ;; ;if r.cns3 eq 'GJ 702B' then l = where(dist lt 50 and w1 gt 0 and w1 lt r.k+3.)
     ;; if n_elements(l) gt 1 then l = where(Dist eq min(dist[where(w1 lt r.k+3.0)]) and (w1 lt r.k+3.))
     ;; l = l[0]
     ;; if l[0] ne -1 then begin
     ;;    ;;l = where(dist lt 50 and w1 gt 0 and w1 lt r.k)
     ;;    ;; w1 = (info.W1MPRO)[l]
     ;;    ;; w1_err = (info.w1sigmpro)[l]
     ;;    ;; w1_sat = (info.w1sat)[l]
     ;;    ;; w2 = (info.W2MPRO)[l]
     ;;    ;; w2_err = (info.w2sigmpro)[l]
     ;;    ;; w2_sat = (info.w2sat)[l]
     ;;    ;; w3 = (info.W3MPRO)[l]
     ;;    ;; w3_err = (info.w3sigmpro)[l]
     ;;    ;; w3_sat = (info.w3sat)[l]
     ;;    ;; w4 = (info.W4MPRO)[l]
     ;;    ;; w4_err = (info.w4sigmpro)[l]
     ;;    ;; w4_sat = (info.w4sat)[l]
     ;;    ;; cc_flag = (info.cc_flags)[l]
     ;;    w1 = w1[l]
     ;;    w1_err = w1_err[l]
     ;;    w1_sat = w1_sat[l]
     ;;    w2 = w2[l]
     ;;    w2_err = w2_err[l]
     ;;    w2_sat = w2_sat[l]
     ;;    w3 = w3[l]
     ;;    w3_err = w3_err[l]
     ;;    w3_sat = w3_sat[l]
     ;;    w4 = w4[l]
     ;;    w4_err = w4_err[l]
     ;;    w4_sat = w4_sat[l]
     ;;    cc_flag = cc_flag[l]
     ;;    ext_flag = ext_flag[l]
     ;;    ph_qual = ph_qual[l]
     ;;    if w1 lt 8 or w2 lt 7 then begin
     ;; info = query_irsa_cat([r.ra+(12.*r.pmra/3600),r.dec+(12.*r.pmdec/3600)], catalog='wise_allsky_4band_p3as_psd', radius=50 )
     ;; if isarray(info) then begin
     ;;    if n_elements(info.ra) gt 1 then begin ;; This needs to be checked and updated
     ;;       gcirc,2,r.ra+(12.*r.pmra/3600),r.dec+(12.*r.pmdec/3600),info.ra,info.dec,dist
     ;;       l = where(dist eq min(dist))
     ;;       if r.cns3 eq 'GJ 105A' then l = 1
     ;;    endif else l = [0]
     ;;    l = l[0]
     ;;    w1 = (info.W1MPRO)[l]
     ;;    w1_err = (info.w1sigmpro)[l]
     ;;    w1_sat = (info.w1sat)[l]
     ;;    w2 = (info.W2MPRO)[l]
     ;;    w2_err = (info.w2sigmpro)[l]
     ;;    w2_sat = (info.w2sat)[l]
     ;;    w3 = (info.W3MPRO)[l]
     ;;    w3_err = (info.w3sigmpro)[l]
     ;;    w3_sat = (info.w3sat)[l]
     ;;    w4 = (info.W4MPRO)[l]
     ;;    w4_err = (info.w4sigmpro)[l]
     ;;    w4_sat = (info.w4sat)[l]
     ;;    cc_flag = (info.cc_flags)[l]

     ;;    str = 'M '+r.othername+' Wise W1 '+string(w1,format="(D7.4)")+' '+string(w1_err,format="(D7.4)")+'  # Wise W1'
     ;;    if finite(w1_err) eq 1 and finite(w1) eq 1 and w1 gt -50 and w1_err gt -50 then printf,25,str
     ;;    str = 'M '+r.othername+' Wise W2 '+string(w2,format="(D7.4)")+' '+string(w2_err,format="(D7.4)")+'  # Wise W2'
     ;;    if finite(w2_err) eq 1 and finite(w2) eq 1 and w2 gt -50 and w2_err gt -50 then printf,25,str
     ;;    str = 'M '+r.othername+' Wise W3 '+string(w3,format="(D7.4)")+' '+string(w3_err,format="(D7.4)")+'  # Wise W3'
     ;;    if finite(w3_err) eq 1 and finite(w3) eq 1 and w3 gt -50 and w3_err gt -50 then printf,25,str
     ;;    str = 'M '+r.othername+' Wise W4 '+string(w4,format="(D7.4)")+' '+string(w4_err,format="(D7.4)")+'  # Wise W4'
     ;;    if finite(w4_err) eq 1 and finite(w4) eq 1 and w4 gt -50 and w4_err gt -50 then printf,25,str
     ;; endif

     ;; APASS
     if r.v gt 9 or r.v le 0 or finite(r.v) eq 0 then begin
        info = queryvizier('II/336',[r.ra,r.dec],0.05,/all,/silent)
        if isarray(info) then begin
           if n_elements(info) gt 1 then begin
              gcirc,info.RAJ2000,info.dej2000,r.ra,r.dec,dist
              info = info[where(dist eq min(dist))]
              info = info[0]
           endif
                                ;if r.dec gt 0 then begin
        ;   restore,'/Users/amann/Desktop/APASS/apassN.dat'
        ;   apass = apassN
        ;endif else begin
        ;   restore,'/Users/amann/Desktop/APASS/apassS.dat'
        ;   apass = apassS
        ;endelse
        ;gcirc,2,apass.ra,apass.dec,r.ra,r.dec,dist
        ;l = where(dist lt 2.0 or (dist lt 5 and ((abs(r.v-apass.v) lt abs(apass.v_err)*3.0))))
        ;if n_elements(l) gt 1 then begin
        ;   d = d[l]
        ;   l = l[wherE(d eq min(d))]
        ;endif
        ;if l[0] ne -1 then begin
        ;   apass = apass[l]
           if info.vmag gt 10.0 then begin
              b = info.bmag
              b_err = sqrt(info.e_Bmag^2.+0.03^2.)
              if info.e_Bmag lt 0 then b_err = 0.1
              v = info.vmag
              v_err = sqrt(info.e_vmag^2.+0.03^2.)
              if info.e_vmag lt 0 then v_err = 0.1
              rmag = info.r_mag
              rmag_err = sqrt(info.e_r_mag^2.+0.05^2.)
              if info.e_r_mag lt 0 then rmag_err = 0.1
              str = 'M '+r.othername+' APASS B '+string(b,format="(D6.3)")+'  '+string(b_err,format="(D6.3)")+'  # Johnson B'
              if b gt 0 and b lt 20 and b_err gt 0 and b_err lt 1. then printf,25,str
              str = 'M '+r.othername+' APASS V '+string(v,format="(D6.3)")+'  '+string(v_err,format="(D6.3)")+'  # Johnson V'
              if v gt 0 and v lt 20 and v_err gt 0 and v_err lt 1. then printf,25,str
              str = 'M '+r.othername+' APASS r '+string(rmag,format="(D6.3)")+'  '+string(rmag_err,format="(D6.3)")+'  # SDSS r'
              if rmag gt 0 and rmag lt 20 and rmag_err gt 0 and rmag_err lt 1. then printf,25,str
           endif
        endif
     endif

     ;; SDSS
     info = queryvizier('V/139/sdss9',[r.ra,r.dec],0.05,/all,/silent)
     info = info[0]
     if isarray(info) then begin
        u = info.umag
        eu = info.e_umag
        g = info.gmag
        eg = info.e_gmag
        rm = info.rmag
        er = info.e_rmag
        i = info.imag
        ei = info.e_imag
        z = info.zmag
        ez = info.e_zmag
        ;;if finite(u) and u gt 0 then printf,25,'M '+r.othername+' SDSS r '+string(r,format="(D6.3)")+'  '+string(er,format="(D6.3)")+'  # SDSS r'
        if finite(g) and g gt 0 then printf,25,'M '+r.othername+' SDSS g '+string(g,format="(D6.3)")+'  '+string(sqrt(eg^2.0+0.03^2.),format="(D6.3)")+'  # SDSS g'
        if finite(rm) and rm gt 0 then printf,25,'M '+r.othername+' SDSS r '+string(rm,format="(D6.3)")+'  '+string(sqrt(er^2.0+0.03^2.),format="(D6.3)")+'  # SDSS r'
        if finite(i) and i gt 0 then printf,25,'M '+r.othername+' SDSS i '+string(i,format="(D6.3)")+'  '+string(sqrt(ei^2.0+0.03^2.),format="(D6.3)")+'  # SDSS i'
        if finite(z) and z gt 0 then printf,25,'M '+r.othername+' SDSS z '+string(z,format="(D6.3)")+'  '+string(sqrt(ez^2.0+0.03^2.),format="(D6.3)")+'  # SDSS z'
        

     endif
     
     ;;cmc14
     info = queryvizier('I/304',[r.ra,r.dec],0.05,/all,/silent)
     if isarray(info) then begin
        if n_elements(info) gt 1 then begin
           gcirc,info.RAJ2000,info.dej2000,r.ra,r.dec,dist
           info = info[where(dist eq min(dist))]
           info = info[0]
        endif
        rmag = info.r_mag
        rmag_err = sqrt(info.u_r_mag^2.+0.03^2.)
        str = 'M '+r.othername+' CMC14 r '+string(rmag,format="(D6.3)")+'  '+string(rmag_err,format="(D6.3)")+'  # SDSS r'
        if rmag gt 0 and rmag lt 20 and rmag_err gt 0 and rmag_err lt 1. then printf,25,str
     endif
     ;; this is where we can add in extra photometry manually:
     if r.name eq 'PM_I19539+4424E' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 13.126 0.073  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 15.106 0.021  # Johnson B'
        printf,25,str
     endif
     if r.name eq 'PM_I19220+0702' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 12.414 0.017  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 14.056 0.045  # Johnson B'
        printf,25,str
     endif
     if r.name eq 'PM_I00115+5908' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 15.859 0.03  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 17.195 0.03  # Johnson B'
        printf,25,str
     endif
     if r.name eq 'PM_I01186-0052S' then begin ;; manul tycho because of binary
        str = 'M '+r.othername+' Tycho Vt 10.776 0.097  # Tycho Vt'
        printf,25,str
        str = 'M '+r.othername+' Tycho Bt 11.786 0.151  # Tycho Bt'
        printf,25,str
     endif
     if r.name eq 'PM_I01056+2829' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 14.839 0.03  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 16.65 0.02  # Johnson B'
        printf,25,str
     endif    
     if r.name eq 'PM_I01076+2257E' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 15.814 0.05  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 16.873 0.05  # Johnson B'
        printf,25,str
     endif
     if r.name eq 'PM_I02441+4913W' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 11.076 0.024  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 11.335 0.042  # Johnson B'
        printf,25,str
     endif
     if r.name eq 'PM_I18411+2447S' then begin
        str = 'M '+r.othername+' Johnson B 14.0 0.1  # Johnson B'
        printf,25,str
        str = 'M '+r.othername+' Johnson V 12.4 0.1  # Johnson V'
        printf,25,str
     endif
     if r.name eq 'PM_I23455-1610' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 14.386 0.066  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 16.224 0.159  # Johnson B'
        printf,25,str
     endif
     if r.name eq 'PM_I13062+2043' then begin
        printf,25,'# Manual photometry from 2013MNRAS.431.2745G'
        str = 'M '+r.othername+' Johnson J 6.89 0.02  # Johnson J'
        printf,25,str
        str = 'M '+r.othername+' Johnson H 6.27 0.02  # Johnson H'
        printf,25,str
        str = 'M '+r.othername+' Johnson K 6.05 0.02  # Johnson K'
        printf,25,str
     endif
     if r.name eq 'PM_I04073-2429' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 12.388 0.03  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 13.89 0.03  # Johnson B'
        printf,25,str
     endif
     if r.name eq 'PM_I16570-0420' then begin
        printf,25,'# APASS Manual photometry'
        str = 'M '+r.othername+' APASS V 12.278 0.015  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' APASS B 13.871 0.02  # Johnson B'
        printf,25,str
     endif
     if r.name eq 'PM_I02530+1652' then begin
        str = 'M '+r.othername+' Johnson V 15.19 0.11  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' Johnson B 16.856 0.08  # Johnson B'
        printf,25,str
     endif
     if r.cns3 eq 'GJ 338B' then begin
        str = 'M '+r.othername+' Johnson B 9.04 0.05  # Johnson B'
        printf,25,str
        str = 'M '+r.othername+' Johnson V 7.70 0.05  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' Johnson J 4.89 0.05  # Johnson J'
        printf,25,str
        str = 'M '+r.othername+' Johnson H 4.30 0.05  # Johnson H'
        printf,25,str
        str = 'M '+r.othername+' Johnson K 4.15 0.05  # Johnson K'
        printf,25,str
        str = 'M '+r.othername+' Johnson L 4.10 0.05  # Johnson K'
        printf,25,str
        str = 'M '+r.othername+' 2Mass J 4.778 0.17  # 2Mass J'
        printf,25,str
        str = 'M '+r.othername+' 2Mass H 4.043 0.21  # 2Mass H'
        printf,25,str
        str = 'M '+r.othername+' 2Mass K 4.136 0.02  # 2Mass K'
        printf,25,str
     endif
     if r.cns3 eq 'GJ 338A' then begin
        str = 'M '+r.othername+' Johnson B 9.05 0.05  # Johnson B'
        printf,25,str
        str = 'M '+r.othername+' Johnson V 7.64 0.05  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' Johnson J 4.78 0.05  # Johnson J'
        printf,25,str
        str = 'M '+r.othername+' Johnson H 4.25 0.05  # Johnson H'
        printf,25,str
        str = 'M '+r.othername+' Johnson K 4.09 0.05  # Johnson K'
        printf,25,str
        str = 'M '+r.othername+' Johnson L 4.00 0.05  # Johnson K'
        printf,25,str
     endif
     if r.cns3 eq 'GJ 570A' then begin
        str = 'M '+r.othername+' Johnson J 3.82 0.05  # Johnson J'
        printf,25,str
        str = 'M '+r.othername+' Johnson H 3.27 0.05  # Johnson H'
        printf,25,str
        str = 'M '+r.othername+' Johnson K 3.15 0.05  # Johnson K'
        printf,25,str
        str = 'M '+r.othername+' Johnson L 3.11 0.05  # Johnson L'
        printf,25,str
        str = 'M '+r.othername+' Johnson J 3.84 0.05  # Johnson J'
        printf,25,str
        str = 'M '+r.othername+' Johnson K 3.06 0.05  # Johnson K'
        printf,25,str
        str = 'M '+r.othername+' Alexander m608 3.62 0.05  # Alexander m608'
        printf,25,str
        str = 'M '+r.othername+' Alexander m683 4.23 0.05  # Alexander m683'
        printf,25,str
        str = 'M '+r.othername+' Alexander m710 4.39 0.05  # Alexander m710'
        printf,25,str
        str = 'M '+r.othername+' Alexander m746 4.77 0.05  # Alexander m746'
        printf,25,str
     endif
     if r.cns3 eq 'GJ 702B' then begin
        str = 'M '+r.othername+' Johnson B 7.15 0.05  # Johnson B'
        printf,25,str
        str = 'M '+r.othername+' Johnson V 6.00 0.05  # Johnson V'
        printf,25,str
        str = 'M '+r.othername+' Johnson K 3.10 0.05  # Johnson K'
        printf,25,str
     endif
     if r.cns3 eq 'GJ 725A' then begin
        str = 'M '+r.othername+' Alexander m608 6.6 0.05  # Alexander m608'
        printf,25,str
        str = 'M '+r.othername+' Alexander m683 7.19 0.05  # Alexander m683'
        printf,25,str
        str = 'M '+r.othername+' Alexander m710 7.02 0.05  # Alexander m710'
        printf,25,str
        str = 'M '+r.othername+' Alexander m746 6.72 0.05  # Alexander m746'
        printf,25,str
        str = 'M '+r.othername+' Tycho Bt 10.897 0.048  # Tycho Bt'
        printf,25,str
        str = 'M '+r.othername+' Tycho Vt 9.109 0.018  # Tycho Vt'
        printf,25,str

     endif
     if r.cns3 eq 'GJ 725B' then begin
        str = 'M '+r.othername+' Tycho Bt 11.809 0.090  # Tycho Bt'
        printf,25,str
        str = 'M '+r.othername+' Tycho Vt 9.964 0.032  # Tycho Vt'
        printf,25,str
        str = 'M HD\ 173740 Hipparcos Hp  9.9964 0.0474  # Hipparcos Hp'
        printf,25,str

        ;; str = 'M '+r.othername+' Johnson B 11.28 0.05  # Johnson B'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson B 11.24 0.05  # Johnson B'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson B 11.28 0.05  # Johnson B'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson B 11.28 0.05  # Johnson B'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson B 11.28 0.05  # Johnson B'
        ;; printf,25,str

        ;; str = 'M '+r.othername+' Johnson V 9.69 0.05  # Johnson V'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson V 9.67 0.05  # Johnson V'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson V 9.69 0.05  # Johnson V'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson V 9.69 0.05  # Johnson V'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson V 9.69 0.05  # Johnson V'
        ;; printf,25,str

        ;; str = 'M '+r.othername+' Johnson U 12.42 0.05  # Johnson U'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson U 12.33 0.05  # Johnson U'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson U 12.42 0.05  # Johnson U'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson U 12.42 0.05  # Johnson U'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson U 12.42 0.05  # Johnson U'
        ;; printf,25,str

        ;; str = 'M '+r.othername+' Johnson B 11.28 0.05  # Johnson B'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson V 9.69 0.05  # Johnson V'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson J 5.72 0.05  # Johnson J'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson H 5.24 0.05  # Johnson H'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson J 4.98 0.05  # Johnson K'
        ;; printf,25,str
        ;; str = 'M '+r.othername+' Johnson L 4.70 0.05  # Johnson J'
        ;; printf,25,str

        ;; str = 'M HD 173740 Geneva V 9.708 0.08  # Geneva V'
        ;; printf,25,str
        ;; str = 'M HD_173740--M3.5 Geneva V1 10.508 0.08  # Geneva V1'
        ;; printf,25,str
        ;; str = 'M HD_173740--M3.5 Geneva B 10.726 0.08  # Geneva B'
        ;; printf,25,str
        ;; str = 'M HD_173740--M3.5 Geneva B1 12.104 0.08  # Geneva B1'
        ;; printf,25,str
        ;; str = 'M HD_173740--M3.5 Geneva B2 11.784 0.08  # Geneva B2'
        ;; printf,25,str
        ;; str = 'M HD_173740--M3.5 Geneva U 13.115 0.08  # Geneva U'
        ;; printf,25,str
        ;; str = 'M HD_173740--M3.5 Geneva G 10.641 0.08  # Geneva G'
        ;; printf,25,str
     endif
     close,25
  endif
END
