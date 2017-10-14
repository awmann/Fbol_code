pro get_uh22_wave_flux, tt, wave,flux,header, var=var
  header = *tt.uh22header

  naxis1 = sxpar(header, 'NAXIS1')
  crpix1 = sxpar(header, 'CRPIX1')
  crval1 = sxpar(header, 'CRVAL1')
  cdelt1 = sxpar(header, 'CDELT1')

  wave = crval1 + cdelt1*((dindgen(naxis1)+1)-crpix1)      
  flux = *tt.uh22spec

  if keyword_set(var) then var=*tt.uh22var

end



