pro get_mdm_wave_flux, tt, wave,flux,header,var=var
header = *tt.mdmheader

naxis1 = sxpar(header, 'NAXIS1')
crpix1 = sxpar(header, 'CRPIX1')
crval1 = sxpar(header, 'CRVAL1')
cdelt1 = sxpar(header, 'CDELT1')

wave = crval1 + cdelt1*((dindgen(naxis1)+1)-crpix1)      
flux = *tt.mdmspec

if keyword_set(var) then var=*tt.mdmvar




end
