PRO gcal_missing,name = name,redo=redo,add=add

  set_plot,'x'
  if n_elements(redo) eq 0 then redo = 0
  device,retain=2
  restore,'~/Dropbox/structures/rdtargets.dat'
  
  l = where(rdtargets.cns3 eq 'GJ 876') & if l[0] ne -1 then rdtargets[l].othername = 'HIP\ 113020'
  l = where(rdtargets.cns3 eq 'GJ 176') & if l[0] ne -1 then rdtargets[l].othername = 'HD285968'
  l = where(rdtargets.cns3 eq 'GJ 649') & if l[0] ne -1 then rdtargets[where(rdtargets.cns3 eq 'GJ 649')].othername = 'Gliese\ 649'
  l = where(rdtargets.cns3 eq 'GJ752A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ752A')].othername = 'HD\ 180617'
  l = where(rdtargets.cns3 eq 'GJ 15A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 15A')].othername = 'HD\ 001326'
  l = where(rdtargets.cns3 eq 'GJ 699') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 699')].othername = '0980140024'
  l = where(rdtargets.cns3 eq 'GJ 338A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 338A')].othername = '0100079210'
  l = where(rdtargets.cns3 eq 'GJ 412A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 412A')].othername = '0004402051'
  l = where(rdtargets.cns3 eq 'GJ 570A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 570A')].othername = 'HD\ 131977'
  l = where(rdtargets.cns3 eq 'GJ 820A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 820A')].othername = 'HD\ 201091'
  l = where(rdtargets.cns3 eq 'GJ 820B') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 820B')].othername = 'HD\ 201092'
  l = where(rdtargets.cns3 eq 'GJ 105A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 105A')].othername = 'HD\ 016160'
  l = where(rdtargets.cns3 eq 'GJ 702B') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 702B')].othername = 'HD\ 165341'
  l = where(rdtargets.cns3 eq 'GJ 725A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 725A')].othername = 'HD\ 173739'
  l = where(rdtargets.cns3 eq 'GJ 725B') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 725B')].othername = 'HD\ 173739\ B'
  l = where(rdtargets.cns3 eq 'GJ 896A') & if l[0] ne -1 then rdtargets[where(Rdtargets.cns3 eq 'GJ 896A')].othername = 'Gliese\ 896'
  l = where(rdtargets.cns3 eq 'GJ 208') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 245409'
  l = where(rdtargets.name eq 'HD_209458') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 209458'
  l = where(rdtargets.name eq 'HD_189733') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 189733'
  l = where(rdtargets.name eq 'GJ_702A') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 165341'

  ;; added Jan 2015
  l = where(rdtargets.name eq 'GJ166A') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 26965'
  l = where(rdtargets.name eq 'GJ631') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 149661'
  l = where(rdtargets.name eq 'GJ_706') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 166620'
  l = where(rdtargets.name eq 'GJ_105A') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 16160'
  l = where(rdtargets.name eq 'PM_I09144+5241') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 79211'
  l = where(rdtargets.name eq 'PM_I14513+1906') & if l[0] ne -1 then rdtargets[l].othername = 'HD\ 131156'
  l = where(strtrim(rdtargets.name,2) eq 'HD10700') & if l[0] ne -1 then begin
     rdtargets[l].othername = 'HD\ 10700'
     rdtargets[l].spt = 'g8v'
  endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'HD7924') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 10700'
  ;;    rdtargets[l].spt = 'k0v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_145675')] ;HD_145675;HD_10476
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    c = coolstars[where(coolstars.name eq 'HD_10476')] ;HD_145675;HD_10476
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'HD10700') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 10700'
  ;;    rdtargets[l].spt = 'g8v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_101501')] ;HD_75732
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ 451') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 103095'
  ;;    rdtargets[l].spt = 'k1v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_10476')]
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ166A') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 026976'
  ;;    rdtargets[l].spt = 'k1v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_10476')] ;HD_145675;HD_10476
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ75') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 010780'
  ;;    rdtargets[l].spt = 'g8v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_101501')] ;HD_145675;HD_10476
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ 675') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 158633'
  ;;    rdtargets[l].spt = 'k0v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_101501')] ;HD_145675;HD_10476
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ_702A') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 165341'
  ;;    rdtargets[l].spt = 'k0v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_145675')] ;HD_145675;HD_10476
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ 68') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 10476'
  ;;    rdtargets[l].spt = 'k1v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c1 = coolstars[where(coolstars.name eq 'HD_10476')]  ;HD_145675;HD_10476
  ;;    c2 = coolstars[where(coolstars.name eq 'HD_145675')] ;HD_145675;HD_10476
  ;;    combine,c1,c2,arr
  ;;    rdtargets[l].irtfspec = ptr_new(arr)
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ53A') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 6582'
  ;;    rdtargets[l].spt = 'k1v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_145675')] ;HD_145675;HD_10476
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ 33') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 4628'
  ;;    rdtargets[l].spt = 'k2v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_3765')] ; HD_3765 = k2, HD_219134 = k3
  ;;    c1 = coolstars[where(coolstars.name eq 'HD_3765')] 
  ;;    c2 = coolstars[where(coolstars.name eq 'HD_145675')] 
  ;;    combine,c1,c2,arr
  ;;    rdtargets[l].irtfspec = ptr_new(arr)
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ505A') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 115404'
  ;;    rdtargets[l].spt = 'k2v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_10476')] ;HD_3765;HD_219134
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ144') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 22049'
  ;;    rdtargets[l].spt = 'k2v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_10476')] ;HD_3765;HD_219134
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ183') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 32147'
  ;;    rdtargets[l].spt = 'k3v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c1 = coolstars[where(coolstars.name eq 'HD_3765')]   ;HD_3765;HD_219134
  ;;    c2 = coolstars[where(coolstars.name eq 'HD_219134')] ;HD_3765;HD_219134
  ;;    combine,c1,c2,arr
  ;;    rdtargets[l].irtfspec = ptr_new(arr)
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ 408') & if l[0] ne -1 then begin
  ;;    rdtargets[l].hip = 53767
  ;;    rdtargets[l].othername = 'Gliese\ 408'
  ;;    rdtargets[l].spt = 'm3v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'Gl_388')] ;Gl_273  Gl_388
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ764') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 185144'
  ;;    rdtargets[l].spt = 'k0v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_145675')] ;HD_75732 HD_101501 HD_145675
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ764') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 185144'
  ;;    rdtargets[l].spt = 'k0v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[where(coolstars.name eq 'HD_145675')] ;HD_75732 HD_101501 HD_145675
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.name,2) eq 'GJ614') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 145675'
  ;;    rdtargets[l].spt = 'k0v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[wherE(coolstars.name eq 'HD_145675')] 
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ 559B') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 128621'
  ;;    rdtargets[l].spt = 'k1v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[wherE(coolstars.name eq 'HD_145675')] 
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ 845') & if l[0] ne -1 then begin
  ;;    rdtargets[l].othername = 'HD\ 209100'
  ;;    rdtargets[l].spt = 'k5v'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[wherE(coolstars.name eq 'HD_45977')]  ;;HD_36003
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    restore,'~/Dropbox/Structures/targets6.dat'
  ;;    c = targets[wherE(targets.name eq 'PM_I04139+0639')]
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ 551') & if l[0] ne -1 then begin ;; FUCKING DISASTER
  ;;    rdtargets[l].othername = 'Gliese\ 551'
  ;;    rdtargets[l].spt = 'M6V'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[wherE(coolstars.name eq 'Gl_866ABC')]  ;;Gl_406,GJ_1111, Gl_51,Gl_866ABC
  ;;    restore,'~/Dropbox/Structures/targets6.dat'
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ117') & if l[0] ne -1 then begin 
  ;;    rdtargets[l].spt = 'K1V'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c1 = coolstars[wherE(coolstars.name eq 'HD_10476')] ;; HD_10476 = K1, HD_3765 = k2, HD_145675 = k0
  ;;    c2 = coolstars[wherE(coolstars.name eq 'HD_145675')] ;; HD_10476 = K1, HD_3765 = k2, HD_145675 = k0
  ;;    combine,c1,c2,arr
  ;;    c = coolstars[wherE(coolstars.name eq 'HD_10476')] ;; HD_10476 = K1, HD_3765 = k2, HD_145675 = k0
  ;;    rdtargets[l].irtfspec = ptr_new(arr)
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ302') & if l[0] ne -1 then begin 
  ;;    rdtargets[l].spt = 'G8V'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c = coolstars[wherE(coolstars.name eq 'HD_101501')] ;; HD_10476 = K1, HD_3765 = k2, HD_145675 = k0
  ;;    rdtargets[l].irtfspec = c.irtfspec
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  ;; l = where(strtrim(rdtargets.cns3,2) eq 'GJ295') & if l[0] ne -1 then begin 
  ;;    rdtargets[l].spt = 'K0V'
  ;;    restore,'~/Dropbox/Structures/coolstars.dat'
  ;;    c1 = coolstars[wherE(coolstars.name eq 'HD_10476')]  ;; HD_10476 = K1, HD_3765 = k2, HD_145675 = k0, HD_101501 = G8
  ;;    c2 = coolstars[wherE(coolstars.name eq 'HD_10476')]  ;; HD_10476 = K1, HD_3765 = k2, HD_145675 = k0, HD_101501 = G8
  ;;    combine,c1,c2,arr
  ;;    c = coolstars[where(coolstars.name eq 'HD_101501')] ;HD_75732 HD_101501 HD_145675
  ;;    rdtargets[l].irtfspec = ptr_new(arr)
  ;;    rdtargets[l].uh22spec = c.uh22spec
  ;;    rdtargets[l].uh22spec_b = c.uh22spec_b
  ;;    rdtargets[l].uh22var = c.uh22var
  ;;    rdtargets[l].uh22var_b = c.uh22var_b
  ;;    rdtargets[l].uh22header = c.uh22header
  ;;    rdtargets[l].uh22header_b = c.uh22header_b
  ;;    rdtargets[l].uh22snr = 100
  ;;    rdtargets[l].irtfsnr = 100
  ;; endif
  

  qwer = where(rdtargets.stisspec ne ptr_new() and rdtargets.irtfsnr gt 50 and rdtargets.theta gt 0 and rdtargets.name ne 'GJ_702A');;105A gets better agreement with SNIFS, which is very odd, GJ_702A kinda looks like total shit. I think there's a problem with AB being too close and the photometry/spectrum. 
  ;;if n_elements(name) eq 1 then qwer = where(rdtargets.cns3 eq name)
  ;;if qwer[0] eq -1 then qwer = where(rdtargets.name eq name)
  ;;if qwer[0] eq -1 then stop
  h = rdtargets[qwer]          

  oldfbol = dblarr(n_elements(h))
  fancyfbol = dblarr(n_elements(h))
  fancyfbol_err = fancyfbol
  oldmyfbol = dblarr(n_elements(h))
  rchisqs = dblarr(n_elements(h))
  restore,'~/Dropbox/Radii/phot_systems.dat'
  print,n_elements(qwer)
  if n_elements(name) eq '' then name = ' '
  for i = 0,n_Elements(h)-1 do begin
     if h[i].cns3 eq name or name eq ' ' or h[i].name eq name then begin
        gcal,h[i],phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,add=add
        oldfbol[i] = h[i].fbol
        oldmyfbol[i] = h[i].newfbol
        fancyfbol[i] = finalfbol
        fancyfbol_err[i] = finalfbol_err
        rchisqs[i] = rchisq
        l = where(rdtargets.name eq h[i].name)
        if l[0] eq -1 then begin
           print,'PROBLEM'
           stop
        endif
        rdtargets[l].fullspec = ptr_new(finalspec)
        rdtargets[l].fulllambda = ptr_new(finallambda)
        rdtargets[l].fullerr = ptr_new(finalerr)
        rdtargets[l].newfbol = finalfbol
        rdtargets[l].newfbol_err = finalfbol_err
     endif
  endfor
  ;;forprint,h.cns3+string(9b),oldfbol,oldmyfbol*1d12,h.fbol_err,fancyfbol,h.teff,h.newteff,h.teff*(fancyfbol/h.fbol)^(1/4.),/textout
  ;;print,'Program Complete'
  forprint,h.name+string(9b),fancyfbol,fancyfbol_err,rchisqs,/textout
  stop
  save,filename='~/Dropbox/Structures/rdtargets.dat',rdtargets
  
END

PRO proxima

  restore

END
  

PRO usco1610

  restore,'~/Dropbox/Structures/k2.dat'
  k2 = k2[where(k2.name eq 'EPIC_205117205')]
  restore,'~/DRopbox/Structures/young.dat'
  y = young[where(young.uh22snr gt 50 and young.irtfsnr gt 50 and young.uh22numspectype gt 3.1 and young.uh22numspectype lt 3.75 and young.cluster eq 'TWA')]
  y = y[1]

  y.name = 'USco1610'
  y.ra = k2.ra
  y.dec = k2.dec
  y.plx = 1./150.
  y.plx_error = (1./150.)*0.2
  restore,'~/Dropbox/Radii/phot_systems.dat'

  get_struct_wavE_flux,y,waver,specr
  get_struct_wavE_flux,y,waveb,specb,/blue

  readcol, 'savage_mathis_ext_law.txt',invlam,elvebv,skipline=1,format='(F,F)',/silent
  red = 0.4
  svlam = 1./invlam[1:*]
  alebv = 3.1+elvebv[1:*]
  alam = interpol(alebv,svlam,waver/1d4)*red
  redspecr = 10^(-0.4*(-2.5*alog10(specr) + alam))
  alam = interpol(alebv,svlam,waveb/1d4)*red
  redspecb = 10^(-0.4*(-2.5*alog10(specb) + alam))
  y.uh22spec = ptr_new(redspecr)
  y.uh22spec_b = ptr_new(redspecb)
  tmp = *y.irtfspec
  sp = tmp[*,1]
  wave = tmp[*,0]
  alam = interpol(alebv,svlam,wave)*red
  redspecir = 10^(-0.4*(-2.5*alog10(sp) + alam))
  ;;sp[where(wave gt 1.9)]*=1.3
  tmp[*,1] = sp
  y.irtfspec = ptr_new(tmp)

  gcal,y,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,add=add


  stop

END

PRO epic211972086

  restore,'~/Dropbox/MyStructures/othertargets.dat'
  o = othertargets[where(othertargets.name eq 'EPIC_211972086')]
  restore,'~/DRopbox/Structures/young.dat'
  y = young[where(young.uh22snr gt 50 and young.irtfsnr gt 50 and young.uh22numspectype gt 4.5 and young.uh22numspectype lt 5.5)]
  y = y[1]
  struct_assign,o,y
  
  ;;restore,'uscoctio5_mann.idlsave'
  ;;y.name = o.name
  ;;y.ra = o.ra
  ;;y.dec = o.dec
  y.plx = 1./177.
  y.plx_error = y.plx*0.1
  restore,'~/Dropbox/Radii/phot_systems.dat'

  ;;get_struct_wavE_flux,y,waver,specr
  ;;get_struct_wavE_flux,y,waveb,specb,/blue

  ;;readcol, 'savage_mathis_ext_law.txt',invlam,elvebv,skipline=1,format='(F,F)',/silent
  ;;red = 0.08
  ;;svlam = 1./invlam[1:*]
  ;;alebv = 3.1+elvebv[1:*]
  ;;alam = interpol(alebv,svlam,waver/1d4)*red
  ;;redspecr = 10^(-0.4*(-2.5*alog10(specr) + alam))
  ;;alam = interpol(alebv,svlam,waveb/1d4)*red
  ;;redspecb = 10^(-0.4*(-2.5*alog10(specb) + alam))
  ;;y.uh22spec = ptr_new(redspecr)
  ;;y.uh22spec_b = ptr_new(redspecb)
  ;;tmp = *y.irtfspec
  ;;sp = tmp[*,1]
  ;;wave = tmp[*,0]
  ;;alam = interpol(alebv,svlam,wave)*red
  ;;redspecir = 10^(-0.4*(-2.5*alog10(sp) + alam))
  ;;sp[where(wave gt 1.9)]*=1.3
  ;;tmp[*,1] = sp
  ;;y.irtfspec = ptr_new(tmp)

  gcal,y,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,add=add


  stop

END

PRO C13_hyades

  restore,'~/Dropbox/Structures/rdtargets.dat'
  restore,'~/Dropbox/Structures/coolstars.dat'
  restore,'~/Dropbox/Radii/phot_systems.dat'

  ;;cc = coolstars[where(strpos(coolstars.spt,'V') ne -1 and (strpos(coolstars.spt,'M') ne -1 or strpos(coolstars.spt,'K') ne -1) and coolstars.uh22snr gt 100 and coolstars.v gt 0 and coolstars.v-coolstars.j lt 3.5 and strpos(coolstars.spt,'I') eq -1 and strpos(coolstars.spt,'K2') eq -1 and strpos(coolstars.spt,'K0') eq -1 and strpos(coolstars.spt,'K1') eq -1 and strpos(coolstars.spt,'K3') eq -1 and strpos(coolstars.spt,'M2') eq -1 and strpos(coolstars.spt,'M3') eq -1 and strpos(coolstars.spt,'M1') eq -1 and strpos(coolstars.spt,'M0') eq -1 and strpos(coolstars.spt,'K4') eq -1)]
  cc = coolstars[where(coolstars.name eq 'HD_36003')];'HD_201092')]; or coolstars.name eq 'HD_36003')]
  cc = cc[sort(cc.spt)]
  name = []
  chi = []
  spt = []
  forprint,cc.name+string(9b)+cc.spt
  for ii = 0,n_elements(cc)-1 do begin
     c = cc[ii]
     r = rdtargets[0]
     struct_assign,c,r
     r.name = 'LP358-348'
     r.othername = ''
     r.ra = am_racnv('04 29 38.993')
     r.dec = am_deccnv('+22 52 57.80')
     r.irtfsnr = 150
     gcal,r,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=1
     print,c.name+string(9b)+c.spt,rchisq
     chi = [chi,rchisq]
     name = [name,c.name]
     spt = [spt,c.spt]
     print,'-----'
  endfor
  forprint,name,spt,chi
  stop
  

END


PRO gcal,r,phot_systems,finallambda,finalspec,finalerr,finalfbol,finalfbol_err,rchisq,redo=redo,rchisq_nowise=rchisq_nowise,goodpoints=goodpoints,add=add

  set_plot,'x'

  if n_elements(redo) eq 0 then redo = 0
  if n_elements(add) eq 0 then add = 1
  device,retain=2
  close,/all
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
  if r.cns3 eq 'GJ 338A' then rem = 'A'
  if r.cns3 eq 'GJ 338B' then rem = 'B'
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
  if r.name eq 'PM_I09144+5241' then rem = 'B'
  if r.name eq 'PM_I16555-0823' then rem = 'D'
  if r.name eq 'GJ_105A' then rem = 'AP'
  if r.cns3 eq 'GJ 566A' or r.name eq 'GJ 566A' then rem = 'ABV'
  if r.name eq 'GJ166A' then rem = 'A'
  if r.name eq 'GJ_702A' then rem = 'AV'
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
  adjust_spectrum,r,b_fix,b_fix_err,r_fix,r_fix_err,k_fix,k_fix_err,stis=stis
  add_missing_spectrum,r,add=add,compvalues=compvalues
  print,compvalues[6]
  get_spectrum,r,lambda,spec,err,stis=stis

  counter = 0
  while satisfied eq 0 do begin
     offset_thresh = 5 ;; 5
     if r.name eq 'USco1610' or r.name eq 'LP358-348' then offset_thresh = 50000
     counter++
     if counter eq 5 then satisfied = 1
     ;; now everything is in the file, read in that file
     readinphot,r,lambda,spec,err,phot_systems,phot_system,phot_band,phfluxes,phfluxes_err,corr,corr_err,fluxes,fluxes_err,lams,fwhms

     ;; calculation of mastercorr and error
     l = where(phot_band ne 'U' and (phot_band ne 'I' or phot_system ne 'Johnson') and phot_system ne 'Wise')
     mastercorr = median(corr[l])
     offset = abs(corr-mastercorr)/sqrt(corr_err^2.0) ;+mastercorr_Err^2.0)
     l = where(phot_band ne 'U' and (phot_band ne 'I' or phot_system ne 'Johnson') and phot_system ne 'Wise' and offset lt offset_thresh,COMPLEMENT=m)
     mastercorr = wmean(corr[l],corr_err[l],error=mastercorr_Err)
     offset = abs(corr-mastercorr)/sqrt(corr_err^2.0)                                                                                                  ;+mastercorr_Err^2.0)
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

     print,'Blue Correction: '+string(mastercorr_b,format="(D6.3)")+' +/- '+string(mastercorr_b_err,format="(D5.3)")
     print,'Red Correction:  '+string(mastercorr_r,format="(D6.3)")+' +/- '+string(mastercorr_r_err,format="(D5.3)")
     print,'NIR Correction:  '+string(mastercorr_nir,format="(D6.3)")+' +/- '+string(mastercorr_nir_err,format="(D5.3)")

     ;; test to see if the errors are inconsistent at 3-sigma:
     r_b = (mastercorr_r-mastercorr_b)/sqrt(mastercorr_b_err^2.0+mastercorr_r_err^2.0)
     nir_r = (mastercorr_nir-mastercorr_r)/sqrt(mastercorr_r_err^2.0+mastercorr_nir_err^2.0)
     print,'Disagreement: ',r_b,nir_r
     if stis eq 1 then r_b = 0.

     if abs(nir_r) gt 3 and r.name ne 'USco1610' and r.name ne 'LP358-348' then begin
        print,'Fuck'
        stop
     endif

     thresh_nir = 4 ;; maximum difference in standard deviations, important parameter
     thresh_b = 2.0 ;; maximum difference in standard deviations, blue
     if r.name eq 'USco1610' or r.name eq 'LP358-348' then begin
        thresh_b = 5000
        thresh_nir = 5000
     endif
     if r.name eq 'PM_I16148+6038' then thresh_b = 1.5
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
        adjust_spectrum,r,b_fix,b_fix_err,r_fix,r_fix_err,k_fix,k_fix_err
        add_missing_spectrum,r,add=add
        get_spectrum,r,lambda,spec,err,stis=stis
     endif else satisfied = 1
     counter++
     if counter gt 10 then begin
        print,'Failed to converge on reasonable solution'
        stop
     endif
     close,/all
  endwhile
  print,'Final Master correction: '+string(mastercorr,format="(D6.3)")+' +/- '+string(mastercorr_err,format="(D6.3)")

  spec = spec*mastercorr
  err = err*mastercorr

  ;; now fix the RJ tail
  ;; what we want to do is do a RJ fit but also include the photomeric points
  ;; another option is to try extrapolating the models all the way out.
  ;;print,'Fitting Rayleigh-Jeans Tail'
  set_plot,'x'
  !p.multi=[0,1,1]
  q = where(lams gt 2.5)
  if q[0] ne -1 then begin
     ;; clear out the old shitty RJ fit
     g = where(lambda lt 2 or err ne 0)
     spec = spec[g]
     lambda = lambda[g]
     err = err[g]
     ;; now for a new fit!
     f = where(lambda gt 2.6 and err ne 0 and spec)
     if f[0] eq -1 then f = where(lambda gt 2.0 and err ne 0)
     guess = [1d-10,5]
     result = mpfitfun('MYRJL',[lambda[f],lams[q]],[spec[f],phfluxes[q]],[spec[f]/2.,phfluxes_err[q]],guess,perror=perr,/quiet)
     newlambda = generatearray(max(lambda),30.0,100)
     newspec = myrjl(newlambda,result)
     newerr = 0.0*((newspec)*(perr[0]/result[0]))
     ;;plot,lambda,spec,xrange=[2,4]
     ;;oplot,newlambda,newspec,color=cgcolor('red')

     lambda = [lambda,newlambda]
     spec = [spec,newspec]
     err = [err,newerr]
     plot,lambda[where(spec gt 0)],spec[where(spec gt 0)],xrange=[2,30],/xlog,/ylog,/xstyle
     oploterror,lams[q],phfluxes[q],fwhms[q],phfluxes_err[q],psym=8
     off = dblarr(n_elements(q))
     for jj = 0,n_elements(q)-1 do begin
        get_flux,phot_systems,phot_system[q[jj]],phot_band[q[jj]],lambda,spec,err,flux,flux_err
        oplot,[lams[q[jj]]],[flux],psym=8
        off[jj] = (phfluxes[q[jj]]-flux)/phfluxes_err[q[jj]]
     endfor
     f = where(lambda gt 2.6 and err ne 0)
     guess = [1.0,-1.0]
     newlambda = generatearray(max(lambda[where(err ne 0)]),30.0,100)
     nmonte = 500
     bigarr = dblarr(n_elements(newlambda),nmonte)
     newerr = dblarr(n_elements(newlambda))
     for iii = 0,nmonte-1 do begin
        res_temp = result+perr*randomn(seed,2)
        bigarr[*,iii] = MYRJL(newlambda,res_temp)
     endfor
     for jjj = 0,n_elements(newlambda)-1 do begin
        newerr[jjj] = stdev(bigarr[jjj,*])
     endfor
     g = where(err eq 0 and lambda gt 2.0)
     err[g] = newerr   
  endif

  ;; now fix the Wein tail
  ;;print,'Fitting Wein Tail'
  if stis eq 0 then begin ;; do not fit if stis spectrum
     satisfied = 0
     counter = 0
     q = where(lams lt 0.37)
     if n_elements(q) gt 2 then begin ;; need 3 for statistics
        while satisfied eq 0 do begin
           ;; test the Wein fit
           ;;plot,lambda,spec,xrange=[0.3,0.4]
           ;;oploterror,lams[q],phfluxes[q],fwhms[q],phfluxes_err[q],psym=8
           off = dblarr(n_elements(q))
           frac = off
           for jj = 0,n_elements(q)-1 do begin
              get_flux,phot_systems,phot_system[q[jj]],phot_band[q[jj]],lambda,spec,err,flux,flux_err
              ;;oplot,[lams[q[jj]]],[flux],psym=8
              off[jj] = (phfluxes[q[jj]]-flux)^2.0/(phfluxes_err[q[jj]]^2.0+flux_err^2.0)
              frac[jj] = phfluxes[q[jj]]/flux
           endfor
           if median(off) gt offset_thresh then begin
              counter++
              spec[where(lambda lt 0.4 and err eq 0)]*=sqrt((median(frac)))
              satisfied = 0
           endif else begin
              counter++
              spec[where(lambda lt 0.4 and err eq 0)]*=sqrt((median(frac)))
              satisfied = 1
           endelse
           if counter gt 3 then begin
              print,'Failed to converge on Wein'
              satisfied = 1
           endif
           if max(spec[where(lambda lt 0.4 and err eq 0)]) lt min(spec[where(lambda lt 0.4 and err ne 0)]) then satisfied = 1
           if max(spec[where(lambda lt 0.4 and err eq 0)]) gt max(spec[where(lambda lt 0.36 and err ne 0)]) then satisfied = 1
        endwhile
     endif
     f = where(lambda lt 0.5 and err ne 0.0)
     guess = [1.0,-1.0]
     result = mpfitfun('MYWEIN',lambda[f],spec[f],err[f],guess,/quiet,PERROR=weinerr)
     newlambda = generatearray(0.25,min(lambda[where(err ne 0)]),100)
     nmonte = 250
     bigarr = dblarr(n_elements(newlambda),nmonte)
     newerr = dblarr(n_elements(newlambda))
     for iii = 0,nmonte-1 do begin
        res_temp = result+weinerr*randomn(seed,2)
        bigarr[*,iii] = mywein(newlambda,res_temp)
     endfor
     for jjj = 0,n_elements(newlambda)-1 do begin
        newerr[jjj] = stdev(bigarr[jjj,*])
     endfor
     g = where(err eq 0 and lambda lt 0.5)
     err[g] = newerr     
  endif

  ;; re-extract all synthetic photometry:
  fluxes = dblarr(n_elements(phot_system))
  fluxes_err = fluxes
  for jj = 0,n_elements(phot_system)-1 do begin
     get_flux,phot_systems,phot_system[jj],phot_band[jj],lambda,spec,err,flux,flux_err
     fluxes[jj] = flux
     fluxes_err[jj] = flux_err
  endfor
  readinphot,r,lambda,spec,err,phot_systems,phot_system,phot_band,phfluxes,phfluxes_err,corr,corr_err,fluxes,fluxes_err,lams,fwhms
  
  ;; calculation of mastercorr and error
  test_corr = median(corr)
  offset = abs(test_corr-corr)/sqrt(corr_err^2.0) ;+mastercorr_Err^2.0)
  l = where(offset lt offset_thresh and corr_err lt 1,COMPLEMENT=m)
  test_corr2 = wmean(corr[l],corr_err[l],error=test_corr_Err)
  offset = abs(test_corr2-corr)/sqrt(corr_err^2.0) ;+mastercorr_Err^2.0)
  l = where(offset lt offset_thresh,COMPLEMENT=m)
  l_tmp = where((offset lt offset_thresh and offset gt 0) or phot_system eq 'Wise',COMPLEMENT=m2)
  l_nowise = where(offset lt offset_thresh and phot_system ne 'Wise')
  l_cover = where(offset lt offset_thresh and phot_system ne 'Wise' and lams-fwhms gt 0.31)
  rchisq = total((corr[l]-test_corr2)^2.0/corr_err[l]^2.0)/(n_elements(l)-2)
  rchisq_nowise = total((corr[l_nowise]-test_corr2)^2.0/corr_err[l_nowise]^2.0)/(n_elements(l_nowise)-2)
  rchisq_cover = total((corr[l_cover]-test_corr2)^2.0/corr_err[l_cover]^2.0)/(n_elements(l_cover)-2)
  forprint, string(phot_system[l],format="(A10)")+string(9b)+string(phot_band[l],format="(A4)")+string(9b)+string(lams[l],format="(D5.2)"),$
            (corr[l]-test_corr)/corr_err[l], corr[l],corr_err[l], textout='phots/'+name+'.phot',comment='-----------------Goddies-----------------',/silent
  if m[0] ne -1 then $
     forprint,string(phot_system[m],format="(A10)")+string(9b)+string(phot_band[m],format="(A4)")+string(9b)+string(lams[m],format="(D5.2)"),$
              (corr[m]-test_corr)/corr_err[m],(corr[m]),corr_err[m],textout='phots/'+name+'.badphot',comment='-----------------Baddies-----------------',/silent
  print,'Final Secondary Correction: '+string(test_corr2,format="(D6.3)") +' +/- '+string(test_corr_err,format="(D6.3)")
  if m[0] eq -1 then count = 0 else count = n_Elements(m)
  if m2[0] eq -1 then count2 = 0 else count2 = n_Elements(m2)
  print,'Final Reduced Chi^2: '+string(rchisq,format="(D5.2)")+' with '+string(n_elements(l),format="(I3)")+' photometric points.  '+string(count,format="(I3)")+' points excluded. '+string(count2,format="(I2)")+' non-Wise non-I,R points excluded.' 
  print,'Final Reduced Chi^2 without wise: '+string(rchisq_nowise,format="(D5.2)")
  print,'Final Reduced Chi^2 points covered: '+string(rchisq_cover,format="(D5.2)")
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

  nmonte = 200
  fbols = dblarr(nmonte)
  for kk = 0,nmonte-1 do begin
     tmp_spec = spec+err*randomn(seed,n_elements(spec))
     tmp_spec*=(1.+(randomn(seed)*test_corr_err))
     jj = where(finite(tmp_spec) eq 1 and finite(lambda) eq 1)
     fbols[kk] = integral(lambda[jj],tmp_spec[jj])
  endfor
  fbol_err = stdev(fbols)*1d12
  ;; add 0.5% systematic error
  fbol_err=sqrt(fbol_err^2.+(0.005*fbol)^2.)
  print,'Final Fbol: '+string(fbol,format="(D9.4)")+' +/- '+string(fbol_err,format="(D9.4)")
  close,/all


  ;; Plot!
  ;;if add eq 1 then begin
  gcal_plotter,r,phot_system,offset,offset_thresh,name,lambda,spec,err,lams,phfluxes,phfluxes_err,fwhms,FLUXES,FLUXES_err,add=add
  close,/all
  ;;endif

  ;; save the adjusted spectrum
  finalspec = spec
  finallambda = lambda
  finalerr = err
  finalfbol = fbol
  finalfbol_err = fbol_err

  ;; output for kaspar
  n = r.cns3
  if strtrim(n,2) eq '' then n = r.name
  if r.name eq 'EPIC_211972086' then forprint,finallambda,finalspec,finalerr,textout='EPIC_211972086.dat',/nocomment
  kaspar_format,n,finallambda,finalspec,finalerr

END



PRO adjust_spectrum,h,op_adjust,op_adjust_err,IR_adjust,ir_adjust_err,k_fix,k_fix_err,stis=stis
  
  if n_elements(k_fix) eq 0 or n_elements(k_fix_err) eq 0 then begin
     k_fix = 1.0
     k_fix_err = 99.
  endif
  COMPILE_OPT idl2, HIDDEN
  set_plot,'x'
  !p.multi=[0,1,2]
  snifsfwhm = (7000.0/1000.0)
  c = 299792.458D

  extractdata,h,opspec,operr,oplambda,opspec_b,operr_b,oplambda_b,oprv,irspec,irerr,irlambda,irrv,stis=stis
  ;;if h.cns3 eq 'GJ 725B' then irspec[where(irlambda gt 1.9)]*=0.98
  ;;if h.cns3 eq 'GJ 725A' then irspec[where(irlambda gt 1.9)]*=0.98
  if h.name eq 'HD_189733' then irspec[where(irlambda gt 1.9)]*=0.94
  if h.name eq 'PM_I01186-0052S' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I01402+3147' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I01056+2829' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I04290+2155' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I14201-0937' then irspec[where(irlambda gt 1.9)]*=0.93
  if h.name eq 'PM_I14342-1231' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I16254+5418' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I16509+2227' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.name eq 'PM_I19220+0702' then irspec[where(irlambda gt 1.9)]*=0.96
  if h.name eq 'PM_I20034+2951' then irspec[where(irlambda gt 1.9)]*=0.93
  if h.name eq 'PM_I20260+5834' then irspec[where(irlambda gt 1.9)]*=0.90
  if h.name eq 'LSPM_J0355+5214' then irspec[where(irlambda gt 1.9)]*=0.95
  if h.cns3 eq 'GJ 675' then irspec[where(irlambda gt 1.9)]*=1.05
  if h.name eq 'PM_I11000+2249' then irspec[where(irlambda gt 1.9)]*=1.05
  if h.name eq 'GJ302' then irspec[where(irlambda gt 1.9)]*=1.06

  if h.name eq 'GJ673' then irspec[where(irlambda gt 1.9)]*=0.93
  if k_fix ne 1. and abs((k_fix-1.0)/k_fix_err) gt 1 then begin
     if k_fix gt 1.02 then k_fix = 1.02
     if k_fix lt 0.98 then k_fix = 0.98
     irspec[where(irlambda gt 1.9)]*=k_fix
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
  if h.name eq 'LSPM_J0315+0103' then rv = 0.0
  if h.name eq 'NLTT37349' then rv = 0.0
  oplambda = oplambda/(rv/c+1.)
  if stis eq 0 then oplambda_b = oplambda_b/(rv/c+1.)


  ;; limits = [0.805,0.94]
  ;; right = irspec[where(irlambda gt limits[0] and irlambda lt limits[1])]
  ;; right_err = irerr[where(irlambda gt limits[0] and irlambda lt limits[1])]
  ;; right_lambda = irlambda[where(irlambda gt limits[0] and irlambda lt limits[1])]
  ;; s1 = median(right)
  ;; left = opspec[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; left_err = operr[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; left_lambda = oplambda[where(oplambda gt limits[0] and oplambda lt limits[1])]
  ;; right = interpol(right,right_lambda,left_lambda)
  ;; s2 = median(left)
  ;; change = right/left
  ;; if ir_adjust ne 1 or op_adjust ne 1 then stop


  limits = [0.805,0.88]
  if h.cns3 eq 'GJ 176' then limits = [0.805,0.84]
  if h.name eq 'GJ_105A' then limits = [0.92,0.94] ;limits = [0.86,0.90]
  if h.cns3 eq 'GJ 338A' then limits = [0.805,0.85]
  if h.cns3 eq 'GJ 411' then limits = [0.92,0.94]
  if h.cns3 eq 'GJ 526' then limits = [0.90,0.94]
  if h.cns3 eq 'GJ 15B' then limits = [0.86,0.90]
  if h.name eq 'PM_I11033+3558' then limits = [0.90,0.92]
  if h.name eq 'PM_I01324-2154' then limits = [0.94,0.96]
  if h.name eq 'PM_I01510-0607' then limits = [0.85,0.90]
  if h.name eq 'PM_I02050-1736' then limits = [0.88,0.93]
  if h.name eq 'PM_I02123+0334' then limits = [0.86,0.91]
  if h.name eq 'PM_I04376+5253' then limits = [0.88,0.92]
  if h.name eq 'PM_I04429+1857' then limits = [0.805,0.85]
  if h.name eq 'PM_I05314-0340' then limits = [0.88,0.92]
  if h.name eq 'PM_I05557-2651' then limits = [0.86,0.92]
  if h.name eq 'PM_I06140+5140' then limits = [0.88,0.94]
  if h.name eq 'PM_I06246+2325' then limits = [0.83,0.86]
  if h.name eq 'PM_I07232+4605' then limits = [0.85,0.89]
  if h.name eq 'PM_I07274+0513' then limits = [0.86,0.92]
  if h.name eq 'PM_I00183+4401' then limits = [0.88,0.92]
  if h.name eq 'PM_I05033-1722' then limits = [0.88,0.92]
  if h.name eq 'PM_I07287-0317' then limits = [0.86,0.90]
  if h.name eq 'PM_I10196+1952' then limits = [0.88,0.92]
  if h.name eq 'PM_I10520+1359' then limits = [0.88,0.94]
  if h.name eq 'PM_I13457+1453' then limits = [0.86,0.94]
  if h.name eq 'PM_I17364+6820' then limits = [0.805,0.86]
  if h.name eq 'PM_I19070+2053' then limits = [0.88,0.94]
  if h.name eq 'PM_I19169+0510' then limits = [0.83,0.85]
  if h.name eq 'PM_I20525-1658' then limits = [0.88,0.92]
  if h.name eq 'PM_I20533+6209' then limits = [0.83,0.86]
  if h.name eq 'PM_I22361-0050' then limits = [0.83,0.85]
  if h.name eq 'PM_I22532-1415' then limits = [0.9,0.93]
  if h.name eq 'PM_I22565+1633' then limits = [0.855,0.89]
  if h.name eq 'PM_I23455-1610' then limits = [0.89,0.94]
  if h.name eq 'PM_I23492+0224' then limits = [0.89,0.94]
  if h.name eq 'PM_I06490+3706' then limits = [0.86,0.91]
  if h.name eq 'PM_I22290+0139' then limits = [0.89,0.94]
  if h.name eq 'PM_I22021+0124' then limits = [0.87,0.89]
  if h.name eq 'PM_I00219-3124' then limits = [0.87,0.92]
  if h.name eq 'PM_I01571-1014E' then limits = [0.84,0.87]
  if h.name eq 'PM_I08105-1348' then limits = [0.88,0.94]
  if h.name eq 'PM_I10304+5559' then limits = [0.88,0.94]
  if h.name eq 'PM_I17052-0505' then limits = [0.88,0.94]
  if h.name eq 'PM_I06523-0511' then limits = [0.87,0.91]
  if h.name eq 'PM_I23318+1956W' then limits = [0.87,0.92]
  if h.name eq 'HD_209458' then limits = [0.9,0.92]
  if h.name eq 'PM_I01056+2829' then limits = [0.82,0.85]
  if h.name eq 'PM_I01186-0052S' then limits = [0.83,0.85]
  if h.name eq 'PM_I01433+0419' then limits = [0.83,0.85]
  if h.name eq 'PM_I01402+3147' then limits = [0.72,0.75]
  if h.name eq 'PM_I02164+1335' then limits = [0.90,0.94]
  if h.name eq 'PM_I02358+2013' then limits = [0.88,0.92]
  if h.name eq 'PM_I03095+4543' then limits = [0.88,0.92]
  if h.name eq 'PM_I03181+3815' then limits = [0.88,0.92]
  if h.name eq 'PM_I04290+2155' then limits = [0.88,0.92]
  if h.name eq 'PM_I05421+1229' then limits = [0.88,0.92]
  if h.name eq 'PM_I16254+5418' then limits = [0.855,0.865]
  if h.name eq 'PM_I17095+4340' then limits = [0.88,0.92]
  if h.name eq 'PM_I17198+4142' then limits = [0.85,0.87]
  if h.name eq 'PM_I18453+1851' then limits = [0.88,0.92]
  if h.name eq 'PM_I19220+0702' then limits = [0.88,0.92]
  if h.name eq 'PM_I19396-2645' then limits = [0.86,0.89]
  if h.name eq 'PM_I19539+4424W' then limits = [0.88,0.93]
  if h.name eq 'PM_I20034+2951' then limits = [0.88,0.93]
  if h.name eq 'PM_I20105+0632' then limits = [0.89,0.92]
  if h.name eq 'PM_I20260+5834' then limits = [0.87,0.92]
  if h.name eq 'PM_I22279+5741W' then limits = [0.87,0.92]
  if h.name eq 'PM_I16241+4821' then limits = [0.86,0.88]
  if h.name eq 'PM_I22374+3922' then limits = [0.88,0.92]
  if h.name eq 'PM_I23428+3049' then limits = [0.88,0.92]
  if h.name eq 'PM_I23216+1717' then limits = [0.88,0.92]
  if h.name eq 'PM_I21000+4004E' then limits = [0.88,0.95]
  if h.name eq 'PM_I23318+1956E' then limits = [0.90,0.95]
  if h.name eq 'PM_I22468+4420' then limits = [0.88,0.92]
  if h.name eq 'PM_I11033+3558' then limits = [0.95,1.0]
  if h.name eq 'PM_I11054+4331' then limits = [0.80,0.815]
  if h.name eq 'PM_I07344+6256' then limits = [0.89,0.93]
  if h.name eq 'PM_I02002+1303' then limits = [0.90,0.95]
  if h.name eq 'PM_I17378+1835' then limits = [0.88,0.94]
  if h.name eq 'PM_I17578+4635' then limits = [0.88,0.92]
  if h.name eq 'PM_I16303-1239' then limits = [0.88,0.92]
  if h.name eq 'PM_I01230-1257W' then limits = [0.88,0.92]
  if h.name eq 'LSPM_J0355+5214' then limits = [0.88,0.92]
  if h.name eq 'PM_I05463+0112' then limits = [0.88,0.92]
  if h.name eq 'PM_I15354+6005' then limits = [0.88,0.92]
  if h.name eq 'PM_I18409+3131S' then limits = [0.90,0.92]
  if h.name eq 'PM_I20407+1954' then limits = [0.88,0.92]
  if h.name eq 'PM_I21518+4220E' then limits = [0.86,0.91]
  if h.name eq 'PM_I22160+5439' then limits = [0.88,0.92]
  if h.name eq 'PM_I22524+0954' then limits = [0.90,0.94]
  if h.name eq 'Gl166C' then limits = [0.88,0.92]
  if h.name eq 'PM_I17176+5224' then limits = [0.88,0.92]
  if h.name eq 'PM_I18363+1336S' then limits = [0.73,0.76]
  if h.name eq 'GJ887' then limits = [0.88,0.92]
  if h.name eq 'PM_I02190+2352' then limits = [0.73,0.75]
  if h.name eq 'PM_I14513+1906' then limits = [0.93,0.95]
  if h.name eq 'GJ75' then limits = [0.88,0.95]
  if h.name eq 'PM_I11000+2249' then limits = [0.90,0.92]
  if h.name eq 'HD7924' then limits = [0.85,0.92]
  if h.name eq 'GJ614' then limits = [0.88,0.93]
  if h.name eq 'GJ_702A' then limits = [0.9,1.0]
  if h.name eq 'GJ_33' then limits = [0.88,0.92]
  if h.name eq 'GJ302' then limits = [0.86,0.93]
  if h.name eq 'GJ295' then limits = [0.82,0.86]
  if h.name eq 'HD_103095' then limits = [0.88,0.92]
  if h.name eq 'PM_I06548+3316' then limits = [0.88,0.95]
  if h.name eq 'PM_I07446+0333' then limits = [0.88,0.94]
  if h.name eq 'PM_I08161+0118' then limits = [0.88,0.92]
  if h.name eq 'PM_I10564+0700' then limits = [0.75,0.8]
  if h.name eq 'PM_I10430-0912' then limits = [0.88,0.92]
  if h.name eq 'PM_I12388+1141' then limits = [0.88,0.92]
  if h.name eq 'PM_I15194-0743E' then limits = [0.88,0.94]
  if h.name eq 'PM_I15238+1727' then limits = [0.88,0.94]
  if h.name eq 'PM_I23419+4410' then limits = [0.88,0.94]
  if h.name eq 'PM_I00183+4401' or h.cns3 eq 'GJ 15A' then limits = [0.88,1.0]
  if h.name eq 'LSPM_J2106+3844S' then limits = [0.85,0.90]
  if h.name eq 'EPIC_211972086' then limits = [0.8,0.9]

  ;; test only, remove later
  ;;if h.cns3 eq 'GJ 820A' then limits = [0.88,0.95]
  
  ;;s1 = median(opspec[where(oplambda gt limits[0] and oplambda lt limits[1])])
  ;;s2 = median(irspec[where(irlambda gt limits[0] and irlambda lt limits[1])])
  ;;Coverlap = s2/s1

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

  if h.name eq 'EPIC_211972086' then coverlap*=1.02
  opspec*=Coverlap
  operr*=Coverlap
  print,'Red correction  ',coverlap

  if stis eq 0 then begin
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

     if h.name eq 'PM_I03361+3118' then coverlap*=1.1
     if h.name eq 'PM_I03526+1701' then coverlap*=0.95
     ;;if h.cns3 eq 'GJ 702B' then coverlap*=1.1
     if h.cns3 eq 'GJ 876' then coverlap*=1.05 ;1.15
     if h.cns3 eq 'GJ 649' then coverlap*=0.93
     if h.name eq 'PM_I01528-2226' then coverlap*=1.05
     if h.name eq 'PM_I04073-2429' then coverlap *= 0.89
     if h.name eq 'PM_I04376-1102' then coverlap *= 1.05
     if h.name eq 'PM_I06077-2544' then coverlap *= 1.05
     if h.name eq 'PM_I13457+1453' then coverlap *= 1.25
     if h.name eq 'PM_I16570-0420' then coverlap *=0.85
     if h.name eq 'PM_I20533+6209' then coverlap *=0.85
     if h.name eq 'PM_I23455-1610' then coverlap *=0.90
     if h.name eq 'PM_I23318+1956W' then coverlap *=0.83
     if h.name eq 'PM_I01056+2829' then coverlap *=0.80
     if h.name eq 'PM_I00115+5908' then coverlap *=1.2
     if h.name eq 'PM_I00118+2259' then coverlap*=0.95
     if h.name eq 'PM_I01186-0052S' then coverlap*=1.07
     if h.name eq 'PM_I02164+1335' then coverlap*=0.72
     ;;if h.name eq 'PM_I02190+2352' then coverlap*=1.05
     if h.name eq 'PM_I05421+1229' then coverlap*=1.25
     if h.name eq 'PM_I17095+4340' then coverlap*=0.95
     if h.name eq 'PM_I17115+3826' then coverlap*=0.9
     if h.name eq 'PM_I18453+1851' then coverlap*=0.85
     if h.name eq 'PM_I19220+0702' then coverlap*=0.95
     if h.name eq 'PM_I19539+4424W' then coverlap*=0.9
     if h.name eq 'PM_I20034+2951' then coverlap*=0.93
     if h.name eq 'PM_I20105+0632' then coverlap*=1.25
     if h.name eq 'PM_I20450+4429' then coverlap*=1.1
     if h.name eq 'PM_I06140+5140' then coverlap*=0.9
     if h.name eq 'PM_I09447-1812' then coverlap*=0.95
     if h.name eq 'PM_I18363+1336S' then coverlap*=1.1
     ;;if h.name eq 'EPIC_211972086' then coverlap*=1.5
     ;;if h.name eq 'PM_I23492+0224' then coverlap *=0.90
     ;;coverlap*=op_adjust
     if stis eq 0 then begin
        opspec_b*=Coverlap
        operr_b*=Coverlap
     endif
  endif
  plot,oplambda,opspec,xrange=[0.475,0.55]
  if stis eq 0 then begin
     oplot,oplambda_b,opspec_b,color=cgcolor('blue')
     print,'Blue correction  ',coverlap
  endif

  limits = [0.83,0.9]                              ;;[0.925,0.94];;
  if h.name eq 'GJ_105A' then limits = [0.92,0.94] ;limits = [0.86,0.90]
  if h.name eq 'PM_I11033+3558' then limits = [0.90,0.92]
  if h.cns3 eq 'GJ 338A' then limits = [0.805,0.85]
  if h.cns3 eq 'GJ 411' then limits = [0.92,0.94]
  if h.cns3 eq 'GJ 526' then limits = [0.90,0.94]
  if h.cns3 eq 'GJ 2' then limits = [0.83,0.89]
  if h.cns3 eq 'GJ 15B' then limits = [0.86,0.90]
  if h.cns3 eq 'GJ 1009' then limits = [0.805,0.89]
  if h.name eq 'PM_I01324-2154' then limits = [0.94,0.96]
  if h.name eq 'PM_I01510-0607' then limits = [0.85,0.90]
  if h.name eq 'PM_I02050-1736' then limits = [0.88,0.93]
  if h.name eq 'PM_I02123+0334' then limits = [0.86,0.91]
  if h.name eq 'PM_I04376+5253' then limits = [0.88,0.92]
  if h.name eq 'PM_I04429+1857' then limits = [0.805,0.85]
  if h.name eq 'PM_I05314-0340' then limits = [0.88,0.92]
  if h.name eq 'PM_I05557-2651' then limits = [0.86,0.92]
  if h.name eq 'PM_I06140+5140' then limits = [0.88,0.94]
  if h.name eq 'PM_I06246+2325' then limits = [0.83,0.86]
  if h.name eq 'PM_I07232+4605' then limits = [0.85,0.89]
  if h.name eq 'PM_I07274+0513' then limits = [0.86,0.92]
  if h.name eq 'PM_I00051+4547' then limits = [0.83,0.85]
  if h.name eq 'PM_I00183+4401' then limits = [0.88,0.92]
  if h.name eq 'PM_I05033-1722' then limits = [0.88,0.92]
  if h.name eq 'PM_I07287-0317' then limits = [0.86,0.90]
  if h.name eq 'PM_I10196+1952' then limits = [0.88,0.92]
  if h.name eq 'PM_I10520+1359' then limits = [0.88,0.94]
  if h.name eq 'PM_I13457+1453' then limits = [0.86,0.94]
  if h.name eq 'PM_I17364+6820' then limits = [0.805,0.86]
  if h.name eq 'PM_I19070+2053' then limits = [0.88,0.94]
  if h.name eq 'PM_I19169+0510' then limits = [0.83,0.85]
  if h.name eq 'PM_I20525-1658' then limits = [0.88,0.92]
  if h.name eq 'PM_I20533+6209' then limits = [0.83,0.86]
  if h.name eq 'PM_I22361-0050' then limits = [0.83,0.85]
  if h.name eq 'PM_I22532-1415' then limits = [0.9,0.93]
  if h.name eq 'PM_I22565+1633' then limits = [0.855,0.89]
  if h.name eq 'PM_I23455-1610' then limits = [0.89,0.94]
  if h.name eq 'PM_I23492+0224' then limits = [0.89,0.94]
  if h.name eq 'PM_I06490+3706' then limits = [0.86,0.91]
  if h.name eq 'PM_I22290+0139' then limits = [0.89,0.94]
  if h.name eq 'PM_I22021+0124' then limits = [0.87,0.89]
  if h.name eq 'PM_I00219-3124' then limits = [0.87,0.92]
  if h.name eq 'PM_I01571-1014E' then limits = [0.84,0.87]
  if h.name eq 'PM_I08105-1348' then limits = [0.88,0.94]
  if h.name eq 'PM_I10304+5559' then limits = [0.88,0.94]
  if h.name eq 'PM_I17052-0505' then limits = [0.88,0.94]
  if h.name eq 'PM_I06523-0511' then limits = [0.87,0.91]
  if h.name eq 'PM_I23318+1956W' then limits = [0.87,0.92]
  if h.name eq 'HD_209458' then limits = [0.9,0.92]
  if h.name eq 'PM_I01056+2829' then limits = [0.82,0.85]
  if h.name eq 'PM_I01186-0052S' then limits = [0.83,0.85]
  if h.name eq 'PM_I01433+0419' then limits = [0.83,0.85]
  if h.name eq 'PM_I01402+3147' then limits = [0.72,0.75]
  if h.name eq 'PM_I02164+1335' then limits = [0.90,0.94]
  if h.name eq 'PM_I02358+2013' then limits = [0.88,0.92]
  if h.name eq 'PM_I03095+4543' then limits = [0.88,0.92]
  if h.name eq 'PM_I03181+3815' then limits = [0.88,0.92]
  if h.name eq 'PM_I04290+2155' then limits = [0.88,0.92]
  if h.name eq 'PM_I05421+1229' then limits = [0.88,0.92]
  if h.name eq 'PM_I16254+5418' then limits = [0.855,0.865]
  if h.name eq 'PM_I17095+4340' then limits = [0.88,0.92]
  if h.name eq 'PM_I17198+4142' then limits = [0.85,0.87]
  if h.name eq 'PM_I18453+1851' then limits = [0.88,0.92]
  if h.name eq 'PM_I19220+0702' then limits = [0.88,0.92]
  if h.name eq 'PM_I19396-2645' then limits = [0.86,0.89]
  if h.name eq 'PM_I19539+4424W' then limits = [0.88,0.93]
  if h.name eq 'PM_I20034+2951' then limits = [0.88,0.93]
  if h.name eq 'PM_I20105+0632' then limits = [0.89,0.92]
  if h.name eq 'PM_I20260+5834' then limits = [0.87,0.92]
  if h.name eq 'PM_I22279+5741W' then limits = [0.87,0.92]
  if h.name eq 'PM_I16241+4821' then limits = [0.86,0.88]
  if h.name eq 'PM_I22374+3922' then limits = [0.88,0.92]
  if h.name eq 'PM_I23428+3049' then limits = [0.88,0.92]
  if h.name eq 'PM_I23216+1717' then limits = [0.88,0.92]
  if h.name eq 'PM_I21000+4004E' then limits = [0.88,0.95]
  if h.name eq 'PM_I23318+1956E' then limits = [0.90,0.95]
  if h.name eq 'PM_I22468+4420' then limits = [0.88,0.92]
  if h.name eq 'PM_I11033+3558' then limits = [0.95,1.0]
  if h.name eq 'PM_I11054+4331' then limits = [0.80,0.815]
  if h.name eq 'PM_I07344+6256' then limits = [0.89,0.93]
  if h.name eq 'PM_I02002+1303' then limits = [0.90,0.95]
  if h.name eq 'PM_I17378+1835' then limits = [0.88,0.94]
  if h.name eq 'PM_I17578+4635' then limits = [0.88,0.92]
  if h.name eq 'PM_I16303-1239' then limits = [0.88,0.92]
  if h.name eq 'PM_I01230-1257W' then limits = [0.88,0.92]
  if h.name eq 'LSPM_J0355+5214' then limits = [0.88,0.92]
  if h.name eq 'PM_I05463+0112' then limits = [0.88,0.92]
  if h.name eq 'PM_I15354+6005' then limits = [0.88,0.92]
  if h.name eq 'PM_I18409+3131S' then limits = [0.90,0.92]
  if h.name eq 'PM_I20407+1954' then limits = [0.88,0.92]
  if h.name eq 'PM_I21518+4220E' then limits = [0.86,0.91]
  if h.name eq 'PM_I22160+5439' then limits = [0.88,0.92]
  if h.name eq 'PM_I22524+0954' then limits = [0.90,0.94]
  if h.name eq 'Gl166C' then limits = [0.88,0.92]
  if h.name eq 'PM_I17176+5224' then limits = [0.88,0.92]
  if h.name eq 'PM_I18363+1336S' then limits = [0.73,0.76]
  if h.name eq 'GJ887' then limits = [0.88,0.92]
  if h.name eq 'PM_I02190+2352' then limits = [0.73,0.75]
  if h.name eq 'PM_I14513+1906' then limits = [0.93,0.95]
  if h.name eq 'GJ75' then limits = [0.88,0.95]
  if h.name eq 'PM_I11000+2249' then limits = [0.90,0.92]
  if h.name eq 'HD7924' then limits = [0.85,0.92]
  if h.name eq 'GJ614' then limits = [0.88,0.93]
  if h.name eq 'GJ_702A' then limits = [0.9,1.0]
  if h.name eq 'GJ_33' then limits = [0.88,0.92]
  if h.name eq 'GJ302' then limits = [0.86,0.93]
  if h.name eq 'GJ295' then limits = [0.82,0.86]
  if h.name eq 'HD_103095' then limits = [0.88,0.92]
  if h.name eq 'PM_I06548+3316' then limits = [0.88,0.95]
  if h.name eq 'PM_I07446+0333' then limits = [0.88,0.94]
  if h.name eq 'PM_I08161+0118' then limits = [0.88,0.92]
  if h.name eq 'PM_I10564+0700' then limits = [0.75,0.8]
  if h.name eq 'PM_I10430-0912' then limits = [0.88,0.92]
  if h.name eq 'PM_I12388+1141' then limits = [0.88,0.92]
  if h.name eq 'PM_I15194-0743E' then limits = [0.88,0.94]
  if h.name eq 'PM_I15238+1727' then limits = [0.88,0.94]
  if h.name eq 'PM_I23419+4410' then limits = [0.88,0.94]
  if h.name eq 'PM_I00183+4401' or h.cns3 eq 'GJ 15A' then limits = [0.88,1.0]
  if h.name eq 'LSPM_J2106+3844S' then limits = [0.85,0.90]
  if h.name eq 'EPIC_211972086' then limits = [0.8,0.9]

  ;; test only, remove later
  ;;if h.cns3 eq 'GJ 820A' then limits = [0.88,0.95]

  opspec_o = opspec[where(oplambda gt limits[0] and oplambda lt limits[1])]
  operr_o = operr[where(oplambda gt limits[0] and oplambda lt limits[1])]
  oplambda_o = oplambda[where(oplambda gt limits[0] and oplambda lt limits[1])]
  irspec_o = irspec[where(irlambda gt limits[0] and irlambda lt limits[1])]
  irlambda_o = irlambda[where(irlambda gt limits[0] and irlambda lt limits[1])]
  irerr_o = irerr[where(irlambda gt limits[0] and irlambda lt limits[1])]
  
  ;;irspec_o = gaussfold(irlambda_o,irspec_o,snifsfwhm) ; convolve to expected resolution
  irspec_o = interpol(irspec_o,irlambda_o,oplambda_o)
  numbefore = 1.0*n_elements(irerr_o)
  irerr_o = interpol(irerr_o,irlambda_o,oplambda_o)
  numbafter = 1.0*n_elements(irerr_o)
  irerr_o*=sqrt(numbafter/numbefore) ; we gain Signal by root(N) when we bin up data 
  toterr = (irerr_o + operr_o)
  overlap = (irspec_o*(operr_o/toterr) + opspec_o*(irerr_o/toterr))
  overlap_err = 1.0/sqrt(1.0/(irerr_o^2.0) + 1.0/(operr_o^2.0))
  if h.name eq 'PM_I11033+3558' then begin
     overlap = irspec_o
     overlap_err = irerr_o
  endif

  if stis eq 0 then begin
     masterspec = [opspec_b[where(oplambda_b lt min(oplambda))],opspec[where(oplambda lt limits[0])],overlap,irspec[where(irlambda gt limits[1])]]
     masterlambda = [oplambda_b[where(oplambda_b lt min(oplambda))],oplambda[where(oplambda lt limits[0])],oplambda_o,irlambda[where(irlambda gt limits[1])]]
     mastererr = [operr_b[where(oplambda_b lt min(oplambda))],operr[where(oplambda lt limits[0])],overlap_err,irerr[where(irlambda gt limits[1])]]
  endif else begin
     masterspec = [opspec[where(oplambda lt limits[0])],overlap,irspec[where(irlambda gt limits[1])]]
     masterlambda = [oplambda[where(oplambda lt limits[0])],oplambda_o,irlambda[where(irlambda gt limits[1])]]
     mastererr = [operr[where(oplambda lt limits[0])],overlap_err,irerr[where(irlambda gt limits[1])]]
  endelse

  l = where(finite(masterspec) eq 1 and finite(masterlambda) eq 1 and finite(mastererr) eq 1 and mastererr gt 0)
  masterspec = masterspec[l]
  masterlambda = masterlambda[l]
  mastererr = mastererr[l]
  plot,masterlambda,masterspec,/xstyle,/ystyle,xrange=[0.68,1.1],thick=1
  oplot,oplambda,opspec,color=cgcolor('green')
  oplot,irlambda,irspec,color=cgcolor('red')
  wait,1

  ;; temporary, making plots for John.
  ;!p.multi=[0,1,1]
  ;set_plot,'PS'
  ;name = h.cns3
  ;strreplace,name,' ',''
  ;strreplace,name,' ',''
  ;strreplace,name,' ',''
  ;device,filename=name+'_overlap.eps',/encapsul,/color
  ;plot,oplambda,opspec,xrange=[0.7,1.2],/xstyle,/ystyle,xtitle='Wavelength',ytitle='Flux',xthick=4,ythick=4,charthick=4,charsize=1.5,thick=3
  ;oplot,irlambda,irspec,color=cgcolor('red'),thick=3
  ;device,/close
 

  h.fullspec = ptr_new(masterspec)
  h.fulllambda = ptr_new(masterlambda)
  h.fullerr = ptr_new(mastererr)

END


PRO extractdata,h,opspec,operr,oplambda,opspec_b,operr_b,oplambda_b,oprv,irspec,irerr,irlambda,irrv,stis=stis

  stis = 0
  COMPILE_OPT idl2, HIDDEN

  if h.name eq 'EPIC_211972086' then begin
     x = mrdfits('spec-2280-53680-0050.fits',1,header)
     opspec = x.flux*1d-17
     oplambda = 10.0^x.loglam
     operr = sqrt(x.ivar)*1d-17
     stis = 1
  endif else begin
     ;; does it have an optical spectrum?
     if h.uh22snr gt 50 then begin
        operr = 1
        get_struct_wave_flux, h, oplambda, opspec, var=operr, /restwave
        operr = sqrt(operr)
        operr_b = 1
        get_struct_wave_flux, h, oplambda_b, opspec_b, var=operr_b, /restwave, /blue
        operr_b = sqrt(operr_b)
     endif else begin ;; get a template optical spectrum
        stop
        spawn,'ls ~/Desktop/pickles/*.dat',list
        spt = strlowcase(h.spt)
        qq = where(strpos(list,spt) ne -1)
        if qq[0] eq -1 then stop
        readcol,list[qq[0]],oplambda,opspec
        gg = wherE(oplambda lt 1d4)
        oplambda = oplambda[gg]
        opspec = opspec[gg]
        operr = opspec*0.02
        stis = 1
     endelse
  endelse

  if h.cns3 eq 'GJ 570A' then begin
     opspec_b[where(oplambda_b lt 4000)]*=0.90
  endif
  if h.cns3 eq 'GJ 411' then begin
     l = where(finite(operr) eq 0)
     if l[0] ne -1 then operr[l] = 0.005*opspec[l]
  endif
  if h.cns3 eq 'GJ 15B' then begin
     opspec*=(oplambda/median(oplambda))^0.05
  endif
  if h.cns3 eq 'GJ 79' then begin
     opspec*=(oplambda/median(oplambda))^0.2
  endif
  if h.cns3 eq 'GJ 2' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I02050-1736' then begin
     opspec*=(oplambda/median(oplambda))^(-0.2)
  endif
  if h.name eq 'PM_I02123+0334' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  ;;if h.name eq 'PM_I02442+2531' then begin
  ;;   opspec*=(oplambda/median(oplambda))^(0.1)
  ;;endif
  if h.name eq 'PM_I02534+1724' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I05415+5329' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I06171+0507' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)
  endif
  if h.name eq 'PM_I05033-1722' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)
  endif
  if h.name eq 'PM_I05314-0340' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif  
  if h.name eq 'PM_I10196+1952' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I11033+3558' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)     
  endif
  if h.name eq 'PM_I11421+2642' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)     
  endif
  if h.name eq 'PM_I13062+2043' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)     
  endif
  if h.name eq 'PM_I13457+1453' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)     
  endif
  if h.name eq 'PM_I18498-2350' then begin
     l = where(oplambda gt 0.85)
     opspec[l]*=(oplambda[l]/min(oplambda[l]))^(0.28)     
  endif
  if h.name eq 'PM_I19169+0510' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)     
  endif
  if h.name eq 'PM_I20533+6209' then begin
     opspec*=(oplambda/median(oplambda))^(-0.2)     
  endif
  if h.name eq 'PM_I22361-0050' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)     
  endif
  if h.name eq 'PM_I23492+0224' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)     
  endif
  if h.name eq 'PM_I06490+3706' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)   
  endif
  if h.name eq 'PM_I07287-0317' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)        
  endif
  if h.name eq 'PM_I18051-0301' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)        
  endif
  if h.name eq 'PM_I21092-1318' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)     
  endif
  if h.name eq 'PM_I12194+2822' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)     
  endif
  if h.name eq 'PM_I11417+4245' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)          
  endif
  if h.name eq 'PM_I08526+2818' then begin
     opspec*=(oplambda/median(oplambda))^(-0.15)          
  endif
  if h.name eq 'PM_I11311-1457' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)          
  endif
  if h.name eq 'PM_I12507-0046' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)          
  endif
  if h.name eq 'PM_I20034+2951' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)          
  endif
  if h.name eq 'PM_I01571-1014E' then begin
     opspec*=(oplambda/median(oplambda))^(-0.2)          
  endif
  if h.name eq 'PM_I08105-1348' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)          
  endif
  if h.name eq 'PM_I10304+5559' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)          
  endif
  if h.name eq 'PM_I17052-0505' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)          
  endif
  if h.name eq 'PM_I17355+6140' then begin
     opspec*=(oplambda/median(oplambda))^(0.4)          
  endif
  if h.name eq 'PM_I06523-0511' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)          
  endif
  if h.name eq 'PM_I23318+1956W' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)          
  endif
  if h.name eq 'PM_I01186-0052S' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)          
  endif
  if h.name eq 'PM_I02358+2013' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)          
  endif
  if h.name eq 'PM_I03095+4543' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)          
  endif
  if h.name eq 'PM_I04290+2155' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)          
  endif 
  if h.name eq 'PM_I16241+4821' then begin
     opspec*=(oplambda/median(oplambda))^(0.3)          
  endif
  if h.name eq 'PM_I16554-0819' then begin
     opspec*=(oplambda/median(oplambda))^(0.5)
  endif
  if h.name eq 'PM_I17095+4340' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I18453+1851' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I20105+0632' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I21000+4004E' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I02164+1335' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)
  endif
  if h.name eq 'PM_I17303+0532' then begin
     opspec*=(oplambda/median(oplambda))^(-0.15)
  endif
  if h.name eq 'PM_I23428+3049' then begin
     opspec*=(oplambda/median(oplambda))^(0.2)
  endif
  if h.name eq 'PM_I23318+1956E' then begin
     opspec*=(oplambda/median(oplambda))^(0.5)
  endif
  if h.name eq 'PM_I00115+5908' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I01125-1659' then begin
     opspec*=(oplambda/median(oplambda))^(-0.15)
  endif
  if h.name eq 'PM_I01324-2154' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I17578+4635' then begin
     opspec*=(oplambda/median(oplambda))^(0.075)
  endif
  if h.name eq 'PM_I01230-1257W' then begin
     opspec*=(oplambda/median(oplambda))^(0.05)
  endif
  if h.name eq 'PM_I01076+2257E' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'PM_I14297-6240' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'LSPM_J0355+5214' then begin
     opspec*=(oplambda/median(oplambda))^(-0.1)
  endif
  if h.name eq 'PM_I05463+0112' then begin
     opspec*=(oplambda/median(oplambda))^(-0.2)
  endif
  if h.name eq 'PM_I11046-0413' then begin
     opspec*=(oplambda/median(oplambda))^(0.23)
  endif
  if h.name eq 'PM_I20407+1954' then begin
     opspec*=(oplambda/median(oplambda))^(0.23)
  endif
  if h.name eq 'PM_I22524+0954' then begin
     opspec*=(oplambda/median(oplambda))^(0.05)
  endif
  if h.name eq 'PM_I17176+5224' then begin
     opspec*=(oplambda/median(oplambda))^(0.25)
  endif
  if h.name eq 'PM_I18363+1336S' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'HD7924' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.name eq 'GJ614' then begin
     opspec*=(oplambda/median(oplambda))^(0.1)
  endif
  if h.stisspec ne ptr_new()  then begin
     tmp = *h.stisspec
     oplambda = tmp[*,0]
     opspec = tmp[*,1]
     operr = tmp[*,2]
     stis = 1
  endif 
  if h.name eq 'GJ_105A' then begin
     opspec*=(oplambda/median(oplambda))^(0.15)
  endif

  oplambda /= 10000.0
  if stis ne 1 then oplambda_b /= 10000.0

  if h.irtfsnr gt 10 then begin
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
  endif else begin ;; get a template optical spectrum
     spawn,'ls ~/Desktop/pickles/*.dat',list
     spt = strlowcase(h.spt)
     qq = where(strpos(list,spt) ne -1)
     if qq[0] eq -1 then stop
     irlambda = 0
     f = 0
     while max(irlambda) lt 2.0 do begin
        readcol,list[qq[f]],irlambda,irspec
        gg = wherE(irlambda gt 0.8)
        irlambda = irlambda[gg]
        irspec = irspec[gg]
        irlambda/=1d4
        irerr = irspec*0.02
        irrv = 0
        f++
     endwhile
  endelse
  if h.cns3 eq 'GJ 338A' then begin
     irspec[where(irlambda lt 1.38)]*=0.98
  endif
  if h.cns3 eq 'GJ 526' then begin
     irspec[where(irlambda lt 1.38)]*=0.98
  endif
  if h.name eq 'PM_I02171+3526' then begin
     irspec[where(irlambda gt 1.8)]*=0.98
  endif
  if h.name eq 'PM_I22361-0050' then begin
     irspec[where(irlambda gt 1.8)]*=1.03
  endif
  if h.name eq 'PM_I04376+5253' then begin
     irspec[where(irlambda gt 1.8)]*=0.93
  endif
  if h.name eq 'PM_I02534+1724' then begin
     irspec[where(irlambda gt 1.8)]*=0.91
  endif
  if h.name eq 'PM_I11046-0413' then begin
     irspec[where(irlambda gt 1.8)]*=1.05
  endif
  if h.name eq 'GJ631' then begin
     irspec[where(irlambda gt 1.8)]*=0.92
  endif
  ;;if h.name eq 'PM_I23245+5751S' then begin
  ;;   irspec[where(irlambda gt 1.8)]*=0.98
  ;;endif
  if h.name eq 'PM_I03095+4543' then begin
     irspec[where(irlambda gt 1.8)]*=1.02
  endif
  if h.name eq 'PM_I23318+1956W' then begin
     l = where(irlambda lt 1.4)
     irspec[l]*=0.95
     ;;irspec[l]*=(irlambda[l]/median(irlambda[l]))^(0.2)
  endif
  if h.name eq 'PM_I01186-0052S' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I02164+1335' then begin
     irspec*=(irlambda/median(irlambda))^(0.25)          
  endif
  if h.name eq 'PM_I16254+5418' then begin
     irspec*=(irlambda/median(irlambda))^(-0.3)          
  endif
  if h.name eq 'PM_I17095+4340' then begin
     irspec*=(irlambda/median(irlambda))^(-0.05)          
  endif
  if h.name eq 'PM_I17115+3826' then begin
     irspec*=(irlambda/median(irlambda))^(-0.075)          
  endif
  if h.name eq 'PM_I18165+4533' then begin
     irspec*=(irlambda/median(irlambda))^(-0.05)          
  endif
  if h.name eq 'PM_I18353+4544' then begin
     irspec*=(irlambda/median(irlambda))^(-0.2)          
  endif
  if h.name eq 'PM_I18453+1851' then begin
     irspec*=(irlambda/median(irlambda))^(0.15)          
  endif
  if h.name eq 'PM_I19216+2052' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I16241+4821' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  ;;if h.name eq 'PM_I16554-0819' then begin
  ;;   irspec*=(irlambda/median(irlambda))^(0.1)          
  ;;endif
  if h.name eq 'PM_I22374+3922' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I23428+3049' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I23099+1425W' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  ;;if h.name eq 'PM_I03181+3815' then begin
  ;;   irspec*=(irlambda/median(irlambda))^(-0.1)          
  ;;endif
  if h.name eq 'PM_I21000+4004E' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)          
  endif
  if h.name eq 'PM_I00115+5908' then begin
     irspec*=(irlambda/median(irlambda))^(-0.075)          
  endif
  if h.name eq 'PM_I22468+4420' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)          
  endif
  if h.name eq 'PM_I18046+1354' then begin
     irspec*=(irlambda/median(irlambda))^(0.07)          
  endif
  if h.name eq 'PM_I11033+3558' then begin
     irspec*=(irlambda/median(irlambda))^(0.04)          
  endif
  if h.name eq 'PM_I11054+4331' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I02442+2531' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)          
  endif
  if h.name eq 'PM_I01125-1659' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)          
  endif
  if h.name eq 'PM_I06140+5140' then begin
     irspec*=(irlambda/median(irlambda))^(-0.15)
  endif
  if h.name eq 'PM_I02171+3526' then begin
     irspec*=(irlambda/median(irlambda))^(0.08)
  endif
  if h.name eq 'PM_I07344+6256' then begin
     irspec*=(irlambda/median(irlambda))^(-0.08)
  endif
  if h.name eq 'PM_I23245+5751S' then begin
     irspec*=(irlambda/median(irlambda))^(-0.08)
  endif
  if h.name eq 'PM_I01324-2154' then begin
     irspec*=(irlambda/median(irlambda))^(0.04)
  endif
  if h.name eq 'PM_I17578+4635' then begin
     irspec*=(irlambda/median(irlambda))^(-0.01)
  endif
  if h.name eq 'PM_I19072+2052' then begin
     irspec*=(irlambda/median(irlambda))^(-0.01)
  endif
  if h.name eq 'PM_I01230-1257W' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I03047+6144' then begin
     irspec*=(irlambda/median(irlambda))^(-0.04)
  endif
  if h.name eq 'LSPM_J0355+5214' then begin
     irspec*=(irlambda/median(irlambda))^(-0.06)
  endif
  if h.name eq 'PM_I06461+3233' then begin
     irspec*=(irlambda/median(irlambda))^(-0.15)
  endif
  if h.name eq 'PM_I15354+6005' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'PM_I16148+6038' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq 'PM_I18409+3131S' then begin
     irspec*=(irlambda/median(irlambda))^(-0.09)
  endif
  if h.name eq 'PM_I20407+1954' then begin
     irspec*=(irlambda/median(irlambda))^(-0.16)
  endif
  if h.name eq 'PM_I22160+5439' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq 'PM_I22524+0954' then begin
     irspec*=(irlambda/median(irlambda))^(0.1)
  endif
  if h.name eq 'Gl166C' then begin
     irspec*=(irlambda/median(irlambda))^(-0.15)
  endif
  if h.name eq 'PM_I18419+3149' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'PM_I18363+1336S' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'LSPM_J2106+3844S' then begin
     irspec*=(irlambda/median(irlambda))^(-0.075)
  endif
  if h.name eq 'GJ631' then begin
     irspec*=(irlambda/median(irlambda))^(-0.1)
  endif
  if h.name eq 'PM_I14513+1906' then begin
     irspec*=(irlambda/median(irlambda))^(-0.3)
  endif
  if h.name eq 'HD7924' then begin
     irspec*=(irlambda/median(irlambda))^(0.08)
  endif
  if h.name eq 'LSPM_J2106+3844S' then begin
     irspec*=(irlambda/median(irlambda))^(0.05)
  endif
  if h.name eq 'EPIC_211972086' then begin
     ll = where(irlambda lt 1.35)
     irspec[ll]*=0.98
  endif
  ;if h.name eq 'GJ295' then begin
  ;   irspec*=(irlambda/median(irlambda))^(0.08)
  ;endif
  ;;if h.name eq 'PM_I17176+5224' then begin
  ;;   irspec*=(irlambda/median(irlambda))^(-0.15)
  ;;endif
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


;; used to combine 2 spectra
PRO combine,c1,c2,arr

  tmp1 = *c1.irtfspec
  l1 = tmp1[*,0]
  s1 = tmp1[*,1]
  err1 = tmp1[*,2]
  tmp2 = *c2.irtfspec
  l2 = tmp2[*,0]
  s2 = tmp2[*,1]
  err2 = tmp2[*,2]
  s2 = interpol(s2,l2,l1)
  s = (s1/median(s1)+s2/median(s2))/2.
  arr = [[l1],[s],[err1]]

END
