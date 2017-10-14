pro get_struct_header,tt,header
;EJH
;20111215 
;similar to get_struct_wave_flux, in principle, but only to get hte
;header info

;use this to figure out whether i want MDM or SNIFS spectra -
;in cases with both, just use SNIFS
if tt.obs eq 'MDM' then header = *tt.mdmheader else header = *tt.uh22header


end



