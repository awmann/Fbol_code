PRO mdallax,redo=redo,start=start,finish=finish,bin=bin,add=add,stop=stop

  ;;GJ673, PM_I01528-2226
  !p.multi=[0,1,1]
  !x.margin = [8,6]
  !y.margin=[5,2]
  if n_elements(add) eq 0 then add = 1
  if n_elements(bin) eq 0 then bin = 0
  if n_elements(stop) eq 0 then stop = 0

  if bin eq 1 then restore,'~/Dropbox/Structures/mdallax_bin_conroy.dat' else restore,'~/Dropbox/Structures/mdallax_conroy.dat'

  ;; temporary
  mdallax.mass = 0
  ;;restore,'~/Dropbox/Structures/targets6.dat'
  ;;for i = 0,n_elements(mdallax)-1 do begin
  ;;   gcirc,2,mdallax[i].ra,mdallax[i].dec,targets.ra,targets.dec,dist
  ;;   l = where(dist lt 5 and dist eq min(dist))
  ;;   if l[0] ne -1 then mdallax[i].irrv = targets[l].irtfrv
  ;;endfor


  
  ;;mdallax.mass = 0
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
  verbose = 0
  for i = start,finish-1 do begin
     ;; if mdallax[i].name ne 'LSPM_J2231+4509' and mdallax[i].name ne 'Gl768.1B' and mdallax[i].name ne 'LSPM_J0816+5704' and mdallax[i].name ne 'LSPM_J0045+0015N' and mdallax[i].name ne 'LSPM_J2049+3216W' and (mdallax[i].radius le 0.01 or (i eq start and i gt 0)) or mdallax[i].cns3 eq 'Gl 411' or mdallax[i].cns3 eq 'Gl 699' or mdallax[i].cns3 eq 'Gl 880' or mdallax[i].cns3 eq 'Gl 876' or mdallax[i].cns3 eq 'GJ 411' or mdallax[i].cns3 eq 'GJ 699' or mdallax[i].cns3 eq 'GJ 880' or mdallax[i].cns3 eq 'GJ 876' then begin 
     ;;if mdallax[i].name ne 'poop' then begin; 'PM_I16303-1239' then
     ;;begin ;if mdallax[i].uh22snr gt 150 and
     ;;min((*mdallax[i].irtfspec)[*,0]) lt 0.75 then begin
     if mdallax[i].mass le 0 then begin
        ;;  
        ;;print,string(counter,format="(I3)")+string(9b)+string(i,format="(I3)")+string(9b)+mdallax[i].comment
        counter++
        gcal,mdallax[i],phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add,verbose=verbose
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
        mdallax[i].mass = 1.0
        
        ;;f counter mod 8 eq 6 then begin
        ;;   print,'saving'
        ;;   if bin eq 0 then save,filename='~/Dropbox/Structures/mdallax_conroy.dat',mdallax else save,filename='~/Dropbox/Structures/mdallax_bin_conroy.dat',mdallax
        ;;endif
     endif else begin
     endelse
     
  endfor
  stop
  if bin eq 0 then save,filename='~/Dropbox/Structures/mdallax_conroy.dat',mdallax else save,filename='~/Dropbox/Structures/mdallax_bin_conroy.dat',mdallax

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



PRO gcal,r,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add,long=long,inset=inset,oc=oc,hot=hot,printname=printname,verbose=verbose

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
  adjust_spectrum,r,b_fix,b_fix_err,r_fix,r_fix_err,k_fix,k_fix_err,stis=stis,verbose=verbose
  add_missing_spectrum,r,add=add,compvalues=compvalues,hot=hot,conroy=1,plot=0
  if add eq 1 and verbose eq 1 then print,compvalues[6]
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
     l = where(phot_band ne 'W3' and phot_band ne 'W4')
     phot_band = phot_band[l]
     phfluxes = phfluxes[l]
     phfluxes_err = phfluxes_err[l] 
     corr = corr[l]
     corr_err = corr_err[l]
     fluxes = fluxes[l]
     fluxes_err = fluxes_err[l]
     lams = lams[l]
     fwhms = fwhms[l]
     phot_system = phot_system[l]

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

     if verbose eq 1 then begin
        print,'Blue Correction: '+string(mastercorr_b,format="(D6.3)")+' +/- '+string(mastercorr_b_err,format="(D5.3)")
        print,'Red Correction:  '+string(mastercorr_r,format="(D6.3)")+' +/- '+string(mastercorr_r_err,format="(D5.3)")
        print,'NIR Correction:  '+string(mastercorr_nir,format="(D6.3)")+' +/- '+string(mastercorr_nir_err,format="(D5.3)")
     endif
        
     ;; test to see if the errors are inconsistent at 3-sigma:
     r_b = (mastercorr_r-mastercorr_b)/sqrt(mastercorr_b_err^2.0+mastercorr_r_err^2.0)
     nir_r = (mastercorr_nir-mastercorr_r)/sqrt(mastercorr_r_err^2.0+mastercorr_nir_err^2.0)
     if verbose eq 1 then print,'Disagreement: ',r_b,nir_r
     if stis eq 1 then r_b = 0.

     if abs(nir_r) gt 4 then begin
        print,'Fuck'
        ;;stop
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
        adjust_spectrum,r,b_fix,b_fix_err,r_fix,r_fix_err,k_fix,k_fix_err,verbose=verbose
        add_missing_spectrum,r,add=add,hot=hot,plot=0
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
  if verbose eq 1 then print,'Final Master correction: '+string(mastercorr,format="(D6.3)")+' +/- '+string(mastercorr_err,format="(D6.3)")
  spec = spec*mastercorr
  err = err*mastercorr

  ;; now fix the RJ tail
  ;; what we want to do is do a RJ fit but also include the photomeric points
  ;; another option is to try extrapolating the models all the way out.
  ;;print,'Fitting Rayleigh-Jeans Tail'
  ;; set_plot,'x'
  ;; !p.multi=[0,1,1]
  ;; q = where(lams gt 2.5)
  ;; if q[0] ne -1 and max(lambda) lt 29d0 then begin
  ;;    ;; clear out the old shitty RJ fit
  ;;    g = where(lambda lt 2 or err ne 0)
  ;;    spec = spec[g]
  ;;    lambda = lambda[g]
  ;;    err = err[g]
  ;;    ;; now for a new fit!
  ;;    f = where(lambda gt 2.6 and err ne 0 and spec)
  ;;    if f[0] eq -1 then f = where(lambda gt 2.0 and err ne 0)
  ;;    guess = [1d-10,5]
  ;;    result = mpfitfun('MYRJL',[lambda[f],lams[q]],[spec[f],phfluxes[q]],[spec[f]/2.,phfluxes_err[q]],guess,perror=perr,/quiet)
  ;;    newlambda = generatearray(max(lambda),30.0,100)
  ;;    newspec = myrjl(newlambda,result)
  ;;    newerr = 0.0*((newspec)*(perr[0]/result[0]))
  ;;    ;;plot,lambda,spec,xrange=[2,4]
  ;;    ;;oplot,newlambda,newspec,color=cgcolor('red')

  ;;    lambda = [lambda,newlambda]
  ;;    spec = [spec,newspec]
  ;;    err = [err,newerr]
  ;;    plot,lambda[where(spec gt 0)],spec[where(spec gt 0)],xrange=[2,30],/xlog,/ylog,/xstyle
  ;;    oploterror,lams[q],phfluxes[q],fwhms[q],phfluxes_err[q],psym=8
  ;;    off = dblarr(n_elements(q))
  ;;    for jj = 0,n_elements(q)-1 do begin
  ;;       get_flux,phot_systems,phot_system[q[jj]],phot_band[q[jj]],lambda,spec,err,flux,flux_err
  ;;       oplot,[lams[q[jj]]],[flux],psym=8
  ;;       off[jj] = (phfluxes[q[jj]]-flux)/phfluxes_err[q[jj]]
  ;;    endfor
  ;;    f = where(lambda gt 2.6 and err ne 0)
  ;;    guess = [1.0,-1.0]
  ;;    newlambda = generatearray(max(lambda[where(err ne 0)]),30.0,100)
  ;;    nmonte = 500
  ;;    bigarr = dblarr(n_elements(newlambda),nmonte)
  ;;    newerr = dblarr(n_elements(newlambda))
  ;;    for iii = 0,nmonte-1 do begin
  ;;       res_temp = result+perr*randomn(seed,2)
  ;;       bigarr[*,iii] = MYRJL(newlambda,res_temp)
  ;;    endfor
  ;;    for jjj = 0,n_elements(newlambda)-1 do begin
  ;;       newerr[jjj] = stdev(bigarr[jjj,*])
  ;;    endfor
  ;;    g = where(err eq 0 and lambda gt 2.0)
  ;;    err[g] = newerr   
  ;; endif

  ;; now fix the Wein tail
  ;;print,'Fitting Wein Tail'
  ;; if stis eq 0 then begin ;; do not fit if stis spectrum
  ;;    satisfied = 0
  ;;    counter = 0
  ;;    q = where(lams lt 0.37)
  ;;    if n_elements(q) gt 2 then begin ;; need 3 for statistics
  ;;       while satisfied eq 0 do begin
  ;;          ;; test the Wein fit
  ;;          plot,lambda,spec,xrange=[0.3,0.4]
  ;;          oploterror,lams[q],phfluxes[q],fwhms[q],phfluxes_err[q],psym=8
  ;;          off = dblarr(n_elements(q))
  ;;          frac = off
  ;;          for jj = 0,n_elements(q)-1 do begin
  ;;             get_flux,phot_systems,phot_system[q[jj]],phot_band[q[jj]],lambda,spec,err,flux,flux_err
  ;;             oplot,[lams[q[jj]]],[flux],psym=8
  ;;             off[jj] = (phfluxes[q[jj]]-flux)^2.0/(phfluxes_err[q[jj]]^2.0+flux_err^2.0)
  ;;             frac[jj] = phfluxes[q[jj]]/flux
  ;;          endfor
  ;;          if median(off) gt offset_thresh then begin
  ;;             counter++
  ;;             spec[where(lambda lt 0.4 and err eq 0)]*=sqrt((median(frac)))
  ;;             satisfied = 0
  ;;          endif else begin
  ;;             counter++
  ;;             spec[where(lambda lt 0.4 and err eq 0)]*=sqrt((median(frac)))
  ;;             satisfied = 1
  ;;          endelse
  ;;          if counter gt 3 then begin
  ;;             print,'Failed to converge on Wein'
  ;;             satisfied = 1
  ;;          endif
  ;;          if max(spec[where(lambda lt 0.4 and err eq 0)]) lt min(spec[where(lambda lt 0.4 and err ne 0)]) then satisfied = 1
  ;;          if max(spec[where(lambda lt 0.4 and err eq 0)]) gt max(spec[where(lambda lt 0.36 and err ne 0)]) then satisfied = 1
  ;;       endwhile
  ;;    endif
  ;;    f = where(lambda lt 0.5 and err ne 0.0)
  ;;    guess = [1.0,-1.0]
  ;;    result = mpfitfun('MYWEIN',lambda[f],spec[f],err[f],guess,/quiet,PERROR=weinerr)
  ;;    newlambda = generatearray(0.25,min(lambda[where(err ne 0)]),100)
  ;;    nmonte = 250
  ;;    bigarr = dblarr(n_elements(newlambda),nmonte)
  ;;    newerr = dblarr(n_elements(newlambda))
  ;;    for iii = 0,nmonte-1 do begin
  ;;       res_temp = result+weinerr*randomn(seed,2)
  ;;       bigarr[*,iii] = mywein(newlambda,res_temp)
  ;;    endfor
  ;;    for jjj = 0,n_elements(newlambda)-1 do begin
  ;;       newerr[jjj] = stdev(bigarr[jjj,*])
  ;;    endfor
  ;;    g = where(err eq 0 and lambda lt 0.5)
  ;;    err[g] = newerr     
  ;; endif

  ;; re-extract all synthetic photometry:
  fluxes = dblarr(n_elements(phot_system))
  fluxes_err = fluxes
  for jj = 0,n_elements(phot_system)-1 do begin
     get_flux,phot_systems,phot_system[jj],phot_band[jj],lambda,spec,err,flux,flux_err
     fluxes[jj] = flux
     fluxes_err[jj] = flux_err
  endfor
  readinphot,r,lambda,spec,err,phot_systems,phot_system,phot_band,phfluxes,phfluxes_err,corr,corr_err,fluxes,fluxes_err,lams,fwhms
  l = where(phot_band ne 'W3' and phot_band ne 'W4')
  phot_band = phot_band[l]
  phfluxes = phfluxes[l]
  phfluxes_err = phfluxes_err[l] 
  corr = corr[l]
  corr_err = corr_err[l]
  fluxes = fluxes[l]
  fluxes_err = fluxes_err[l]
  lams = lams[l]
  fwhms = fwhms[l]
  phot_system = phot_system[l]

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
  if verbose eq 1 then print,'Final Secondary Correction: '+string(test_corr2,format="(D6.3)") +' +/- '+string(test_corr_err,format="(D6.3)")
  if m[0] eq -1 then count = 0 else count = n_Elements(m)
  if m2[0] eq -1 then count2 = 0 else count2 = n_Elements(m2)
  if verbose eq 1 then print,'Final Reduced Chi^2: '+string(rchisq,format="(D5.2)")+' with '+string(n_elements(l),format="(I3)")+' photometric points.  '+string(count,format="(I3)")+' points excluded. '+string(count2,format="(I2)")+' non-Wise non-I,R points excluded.' 
  if verbose eq 1 then print,'Final Reduced Chi^2 without wise: '+string(rchisq_nowise,format="(D5.2)")
  if verbose eq 1 then print,'Final Reduced Chi^2 points covered: '+string(rchisq_cover,format="(D5.2)")
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
  if verbose eq 1 then print,'Final Fbol: '+string(fbol,format="(D9.5)")+' +/- '+string(fbol_err,format="(D9.5)")
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


PRO adjust_spectrum,h,op_adjust,op_adjust_err,IR_adjust,ir_adjust_err,k_fix,k_fix_err,stis=stis,verbose=verbose
  
  if n_elements(k_fix) eq 0 or n_elements(k_fix_err) eq 0 then begin
     k_fix = 1.0
     k_fix_err = 99.
  endif
  COMPILE_OPT idl2, HIDDEN
  snifsfwhm = (7000.0/1000.0)
  c = 299792.458D

  extractdata,h,opspec,operr,oplambda,opspec_b,operr_b,oplambda_b,oprv,irspec,irerr,irlambda,irrv,stis=stis
  
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
  ;;oplambda = oplambda/(rv/c+1.)
  ;;oplambda_b = oplambda_b/(rv/c+1.)

  limits = [0.805,0.88]

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
  if verbose eq 1 then print,'Red correction  ',coverlap

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

 
  if stis eq 0 then begin
     opspec_b*=Coverlap
     operr_b*=Coverlap
  endif
 
  if verbose eq 1 and stis eq 0 then print,'Blue correction  ',coverlap

  ;; these limits define the region over which we interpolate/combine
  limits = [0.83,0.9]                              ;;[0.925,0.94];;
  irspec_o = irspec[where(irlambda gt limits[0] and irlambda lt limits[1])]
  irerr_o = irerr[where(irlambda gt limits[0] and irlambda lt limits[1])]
  irlam_o = irlambda[where(irlambda gt limits[0] and irlambda lt limits[1])]

  irspec_base = irspec
  opspec_o = interpol(opspec,oplambda,irlam_o)
  ;; generate a weighting array, this array will be 0 at 0.81, and 1
  ;; and 0.90
  weight = findgen(n_elements(opspec_o))
  weight/=max(weight)
  
  tmp1 = irspec_o*(weight) + opspec_o*(1d0-weight)
  tmp2 = irspec_o/tmp1
  res = smooth(tmp2,15)
  irspec[where(irlambda gt limits[0] and irlambda lt limits[1])]/=res
  
  ;; opspec_o = opspec[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; operr_o = operr[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; oplambda_o = oplambda[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; irspec_o = irspec[where(irlambda gt limits[0] and irlambda lt limits[1])]
  ;; irlambda_o = irlambda[where(irlambda gt limits[0] and irlambda lt limits[1])]
  ;; irerr_o = irerr[where(irlambda gt limits[0] and irlambda lt limits[1])]
  
  ;; irspec_o = interpol(irspec_o,irlambda_o,oplambda_o)
  ;; numbefore = 1.0*n_elements(irerr_o)
  ;; irerr_o = interpol(irerr_o,irlambda_o,oplambda_o)
  ;; numbafter = 1.0*n_elements(irerr_o)
  ;; irerr_o*=sqrt(numbafter/numbefore) ; we gain Signal by root(N) when we bin up data 
  ;; toterr = (irerr_o + operr_o)
  ;; overlap = (irspec_o*(operr_o/toterr) + opspec_o*(irerr_o/toterr))
  ;; overlap_err = 1.0/sqrt(1.0/(irerr_o^2.0) + 1.0/(operr_o^2.0))
  ;; if h.name eq 'PM_I11033+3558' then begin
  ;;    overlap = irspec_o
  ;;    overlap_err = irerr_o
  ;; endif

  cut = 0.81
  masterspec = [opspec_b[where(oplambda_b lt min(oplambda))],opspec[where(oplambda lt cut)],irspec[where(irlambda ge cut)]]
  masterlambda = [oplambda_b[where(oplambda_b lt min(oplambda))],oplambda[where(oplambda lt cut)],irlambda[where(irlambda ge cut)]]
  mastererr = [operr_b[where(oplambda_b lt min(oplambda))],operr[where(oplambda lt cut)],irerr[where(irlambda ge cut)]]

  ;; if stis eq 0 then begin
  ;;   masterspec = [opspec_b[where(oplambda_b lt min(oplambda))],opspec[where(oplambda lt limits[0])],overlap,irspec[where(irlambda gt limits[1])]]
  ;;   masterlambda = [oplambda_b[where(oplambda_b lt min(oplambda))],oplambda[where(oplambda lt limits[0])],oplambda_o,irlambda[where(irlambda gt limits[1])]]
  ;;   mastererr = [operr_b[where(oplambda_b lt min(oplambda))],operr[where(oplambda lt limits[0])],overlap_err,irerr[where(irlambda gt limits[1])]]
  ;; endif else begin
  ;;   masterspec = [opspec[where(oplambda lt limits[0])],overlap,irspec[where(irlambda gt limits[1])]]
  ;;   masterlambda = [oplambda[where(oplambda lt limits[0])],oplambda_o,irlambda[where(irlambda gt limits[1])]]
  ;;   mastererr = [operr[where(oplambda lt limits[0])],overlap_err,irerr[where(irlambda gt limits[1])]]
  ;; endelse

  
  l = where(finite(masterspec) eq 1 and finite(masterlambda) eq 1 and finite(mastererr) eq 1 and mastererr gt 0)
  masterspec = masterspec[l]
  masterlambda = masterlambda[l]
  mastererr = mastererr[l]

  set_plot,'x'
  !p.multi=[0,1,2]
  ;;  plot,oplambda,opspec,xrange=[0.475,0.55]
  ;;   oplot,oplambda_b,opspec_b,color=cgcolor('blue')
  plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[0.68,1.1],thick=1,xtitle='Wavelength (Angstroms)'
  oplot,oplambda,opspec,color=cgcolor('green')
  oplot,irlambda,irspec_base,color=cgcolor('red')
  oplot,[0.81,0.81],[0,1],linestyle=2,thick=3
  ;;oplot,irlam_o,res,thick=2,color=cgcolor('teal')
  ;;oplot,masterlambda,masterspec,thick=1
  !p.multi=[3,3,2]
  plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[0.762,0.774],thick=3
  oplot,[0.7668,0.7668],[0,1d5],linestyle=2,thick=3
  oplot,[0.7702,0.7702],[0,1d5],linestyle=2,thick=3
  plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[1.12,1.15],thick=3
  oplot,[1.1408,1.1408],[0,1d5],linestyle=2,thick=3
  oplot,[1.1385,1.1385],[0,1d5],linestyle=2,thick=3
  plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[2.3,2.35],thick=3
  oplot,[2.3232,2.3232],[0,1d5],linestyle=2,thick=3
  oplot,[2.3385,2.3385],[0,1d5],linestyle=2,thick=3

  ;; set_plot,'PS'
  ;; device,filename='~/Dropbox/Mann/Wavetests/'+h.name+'_'+strtrim(string(irrv,format="(D6.1)"),2)+'.eps',/encapsul,/color
  ;; !p.multi=[0,1,3]
  ;; plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[0.762,0.774],thick=3
  ;; oplot,[0.7668,0.7668],[0,1d5],linestyle=2,thick=3
  ;; oplot,[0.7702,0.7702],[0,1d5],linestyle=2,thick=3
  ;; plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[1.12,1.15],thick=3
  ;; oplot,[1.1408,1.1408],[0,1d5],linestyle=2,thick=3
  ;; oplot,[1.1385,1.1385],[0,1d5],linestyle=2,thick=3
  ;; plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[2.3,2.35],thick=3
  ;; oplot,[2.3232,2.3232],[0,1d5],linestyle=2,thick=3
  ;; oplot,[2.3384,2.3384],[0,1d5],linestyle=2,thick=3
  ;; device,/close

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

  ;; some manual fixes due to bad reduction.
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
