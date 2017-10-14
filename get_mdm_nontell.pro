pro get_mdm_nontell, wave,flux, goodsnifswave, usewave,useflux
;EJH
;20120208
;use the areas where the snifs wavelength is good to find where the
;mdm is also good
;this is poor code and should not be used after i have mdm variance
;spectra


matchthres=1.5
wavedist = dblarr(n_elements(wave))


for i=0,n_elements(wave)-1 do $
   wavedist[i] = min(abs(goodsnifswave-wave[i]))


usewave=wave[where(wavedist le matchthres)]
useflux =flux[where(wavedist le matchthres)]

end
