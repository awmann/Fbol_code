pro get_struct_wave_flux,tt,wave,flux,var=var, header=header,userv=userv, restwave=restwave,inair=inair,blue=blue
;EJH
;20111206

;use this to figure out whether i want MDM or SNIFS spectra -
;in cases with both, just use SNIFS

;this corrects to vacuum, unless specified by the inair keyword
;note that the snifs spectra come at MKO pressure, not stp
;i have an if statement for both mdm and snifs because if, at some
;point, i can account for this, i'll need to treat them differently.

  c = 299792.458D

 if n_elements(blue) ne 0 then begin
     get_uh22_wave_flux_b, tt, wave, flux, header, var=var
     if not(keyword_set(inair)) then airtovac, wave
     if keyword_set(restwave) then wave = wave/(tt.uh22rvraw/c+1.)
  endif else begin
     if tt.obs eq 'MDM' then begin
        get_mdm_wave_flux, tt, wave, flux,header,var=var
        if keyword_set(inair) then vactoair, wave   
        if keyword_set(restwave) then wave = wave/(tt.mdmrvraw/c+1.)
     endif else begin
        get_uh22_wave_flux, tt, wave, flux, header, var=var
        if not(keyword_set(inair)) then airtovac, wave
        if keyword_set(restwave) then wave = wave/(tt.uh22rvraw/c+1.)
     endelse
  endelse
 

  if keyword_set(userv) then wave = wave/(userv/c+1.)
  

end



