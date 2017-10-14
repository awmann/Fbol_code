pro get_uh22_wave_flux_b, tt, wave,flux,header, var=var
  header = *tt.uh22header_b

  naxis1 = sxpar(header, 'NAXIS1')
  crpix1 = sxpar(header, 'CRPIX1')
  crval1 = sxpar(header, 'CRVAL1')
  cdelt1 = sxpar(header, 'CDELT1')

  wave = crval1 + cdelt1*((dindgen(naxis1)+1)-crpix1)      
  flux = *tt.uh22spec_b

  if keyword_set(var) then var=*tt.uh22var_b

end
