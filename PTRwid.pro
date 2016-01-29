
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;                   PTRWID routines
;;;;;;;
;;;;;;;  .FULL_RESET_SESSION
;;;;;;;   RESOLVE_ALL
;;;;;;;   SAVE, /ROUTINES, FILENAME = 'C:/PTRwid/PTRwid.sav'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 PRO ____________PTRwid
 end

PRO PTRwid
   compile_opt idl2
   version='v002' 
   date='Oct_14_2015'
   parfile='C:/PTRwid/'+version+'_'+date+'_par.txt'
   codefile='C:/PTRwid/PTRwid_'+version+'_'+date+'.pro'
   help,/source_files, output=mis
   source=strtrim(string(strmid(mis[where(strpos(mis,'PTRWID_EVENT') eq 0)],12,1111)),2)
   
   inf=file_info('C:/PTRwid/')
   if (inf.directory eq 0) then FILE_MKDIR, 'C:/PTRwid/'
   makecsv,'C:/PTRwid/parfile.txt',parfile
   inf=file_info(parfile)
    info='>  processing info '
   if (inf.exists eq 0) then begin
   
          log=['PTRwid_'+version+' released '+date,'Default parfile has been created:', parfile, '','Values can be adjusted (advanced users only).' ]
         
          ;create default PTRwid_parameter.txt
          PTRwidpar=transpose([$
          'PTR DEFAULT VALUES',$
          '',$
           '_p_drift_default=2.4',$   ; hPa
           '_u_drift_default=600',$   ;volt
           '_udx_default=35',$   ;volt
           '_t_drift_default=50',$ ; celcius
           '_reactionlength=9.6',$ ;reactionlength in cm
           '_reduced_mobility=2.8',$ ;reduced mobility of H3O+ in N2
        
         '',$
        
      
         'CRUDE CALIBRATION',$
          '',$
           '_PeaksToConsider=24',$
           
                  '',$
        
  
          'UNIFIED MASS LIST',$
          '',$
           '_PeakAnalysis=1',$
           '_DefaultRes=4500',$
           '_Desired_Min_Signal=1.0E6',$   ;counts
          '_Max_Time_Gap=10',$    ;minutes
          '',$
          'PS1 _ peak search',$
          '',$
          '_Min_Mass=10',$   ;Da
         '',$
          'COMPOUND CLASS',$
          '',$
          '_N_ox_st=-1',$   ;Nitrogen oxidation state
          '',$
          'MassScsale Calibration Parameter Boundaries',$
          '',$
          '_default_a_1000=7800', '_default_a_8000=17300',$
          '_default_t0_1000=-28500', '_default_t0_8000=750',$
          '_exMin=0.49', '_exMax=0.500000015',$
          
         
          '',$
          '3-POINT MASS-SCALE-CALIBRATION ',$
          '',$
          '_M1a=21.0221', '_M1b=42.034',$
          '_M2a=59.0491', '_M2b=116.906',$
          '_M3a=205.195', '_M3b=355.0731',$
          '_tol_ppm=300',$ ;acceptable tollerance to find peak
          '',$
          'OPTIONAL: CALIBRATION TO FIT MASSLIST',$
          '',$
          '_HardCal=0',$
          ;CH4O H+    ;CH2O2 H+    ;C3H6O H+    ;C2H4O2 H+   ;C3H6O2 H+   ;C4H8O2 H+   ;C5H10O2 H+  ;C6H12O2 H+   ;C7H14O2 H+    ;C8H16O2 H+
          '_M1=33.0335','_M2=47.0128','_M3=59.0491','_M4=61.0284','_M5=75.0441','_M6=89.0597','_M7=103.0754','_M8=117.0910','_M9=131.1067','_M10=145.1223',$ 
          ;C9H18O2 H+  ;C10H20O2 H+   ;C11H22O2 H+   ;C12H24O2 H+   ;C13H26O2 H+   ;C14H28O2 H+  ;C15H30O2 H+    ;C16H32O2 H+   ;C17H34O2 H+   ;C18H36O2 H+
          '_M11=159.1380','_M12=173.1536','_M13=187.1693','_M14=201.1849','_M15=215.2006','_M16=229.2162','_M17=243.2319','_M18=257.2475','_M19=271.2632','_M20=285.2788',$ 
          ;C13H14O8 H+  ;C9H11O13N H+  ;C8H18O15 H+   ;C11H16O13 H+  ;C16H18O10 H+  ;C12H20O13 H+  ;C16H14O13 H+
          '_M21=299.0761','_M22=342.0303','_M23=355.0719','_M24=357.0664','_M25=371.0973','_M26=373.0977','_M27=415.0507','_M28=0','_M29=0','_M30=0',$ 
          '',$
          'PEAK SHAPE',$
          '',$
          '_MinSig=800',$
          '',$ 
          'MIXING RATIO:',$
          '',$ 
          '_k19=3',$
          '_k37=3',$
          '_m38=1',$
          '',$    
          'Transmission Fit:',$
          '',$ 
               

         ' _P0=9.1323863212E-02',$
         ' _P1=4.2137768341E-03',$
         ' _P2=-1.3332441405E-5',$
         ' _P3=2.9151001160E-8',$
         ' _P4=-3.4829484108E-11',$
         ' _P5=2.0979046465E-14',$
         ' _P6=-4.9852341527E-18',$
          '',$ 
       
          'EXPORT OPTIONS:',$
          '',$
          '_ExportOne=0',$
          '_CorrectOverlap=1',$
          '_SaveCsv=1',$
          '_JunkSize=1.0E8',$
          '',$
          'CORRECTIONS:',$
          '',$
          '_CorrectionON=1',$
          '_NonExtendingDeadTime=15',$
          '_ExtendingDeadTime=1.3',$
          
          '',$
            'WIDGET ELEMENTS & SIZE',$
          '',$
          '_base_1x=450',  '_base_1y=700',$
          '_base_2x=230',  '_base_2y=700',$;left column
          '_base_4x=200',  '_base_4y=220',$;engineering data, masslist
          '_base_5x=110',  '_base_5y=220',$
          '_base_6x=110',  '_base_6y=220',$
          '_base_7x=200',  '_base_7y=28',$; label 
          '_base_8x=200',  '_base_8y=45', $
          '_base_9x=200',  '_base_9y=30',$
          '_base_10x=200', '_base_10y=24', $
          '_base_11x=200', '_base_11y=30',$
          '_base_12x=210', '_base_12y=700', $; right column
          '_Text_DataDiry=2', '_Text_DestDiry=2', '_List_Filesy=8',  '_List_EngDaty=15',$
          '_Text_Ind1x=40',   '_Text_Ind1x2=40',  '_Text_Ind2x=40',  '_Text_Ind2x2=40', '_Text_logy=30','_Text_infoy=17',$
          '',$
         
          'End of file'])
          File_copy,source[0],codefile
          makecsv,parfile,PTRwidpar
          
          
   endif else begin
          PTRwidpar=readcsvstr(parfile)
          log=['PTRwid_'+version+' released '+date,'Parfile loaded']
   endelse
  
    
   base_1 = WIDGET_BASE(/Row ,uname='base_1', XSIZE = getpar('base_1x'), YSIZE = getpar('base_1y'))
        base_2=widget_base(base_1,/column,uname='base_2',xsize=getpar('base_2x'),ysize=getpar('base_2y'))
              Label_1=Widget_label(base_2,/Align_left,uname='label_1', value="Data directory (HDF5 data):")
              Text_DataDir = WIDGET_TEXT(base_2, uname="Text_DataDir", VALUE="click the right mouse button to select a TOF DATA directory (HDF5 data)", /EDITABLE,/ALL_EVENTS, /NO_NEWLINE,/wrap , /CONTEXT_EVENTS, YSIZE = getpar('Text_DataDiry'),uvalue=-9999)
                              base_3= WIDGET_BASE(Text_DataDir,  /CONTEXT_MENU,  UNAME="base_3")
                              But_1 = WIDGET_BUTTON(base_3, uname='But_1', VALUE = 'browse',  EVENT_PRO = 'getDataDir')
                              But_111 = WIDGET_BUTTON(base_3, uname='But_111', VALUE = 'last settings',  EVENT_PRO = 'LoadSet')
              Label_2=Widget_label(base_2,/Align_left,uname='label_2',value="Destination directory:")
              Text_DestDir = WIDGET_TEXT(base_2, uname="Text_DestDir", VALUE="   ",/ALL_EVENTS, /NO_NEWLINE,/wrap , YSIZE = getpar('Text_DestDiry'))
              Label_3=Widget_label(base_2,/Align_left,uname='Label_3', value="File Creation times:",uvalue=CREATE_STRUCT('name1',['']))
              List_Files=WIDGET_LIST(base_2,UNAME = 'List_Files',event_pro="SingleFile",ysize=getpar('List_Filesy'))
              base_4=widget_base(base_2,/row,uname='base_4',xsize=getpar('base_4x'),ysize=getpar('base_4y'))
                      base_5=widget_base(base_4,/column,uname='base_5',xsize=getpar('base_5x'),ysize=getpar('base_5y'))
                              Label_4=Widget_label(base_5,/Align_left,uname='Label_4', value="Engineering:")
                              List_EngDat=WIDGET_LIST(base_5,UNAME = 'List_EngDat',event_pro="plot1", ysize=getpar('List_EngDaty'))
                      base_6=widget_base(base_4,/column,uname='base_6',xsize=getpar('base_6x'),ysize=getpar('base_6y'))
                              Label_5=Widget_label(base_6,/Align_left,uname='Label_5', value="Mass peaks:",uvalue=CREATE_STRUCT('masslist',[-9999],'MaxMass',[5]))
                              Drop_1=widget_droplist(base_6,uname='Drop_1',event_pro='setdrop1',value=['no timeline','time vs cps', 'time vs ppb'])
                              List_Masses=WIDGET_LIST(base_6,UNAME = 'List_Masses',event_pro="plot2", ysize=13,xsize=11)
              Label_6=Widget_label(base_2,/Align_left,uname='Label_6', value="Unified Mass List:")
                      But_2 = WIDGET_BUTTON(base_2, uname='But_2',event_pro='UniMassList',value='Unified Mass List',uvalue=CREATE_STRUCT('PeakLoc',[-9999])) 
              base_8=widget_base(base_2,/row,uname='base_8',xsize=getpar('base_8x'),ysize=getpar('base_8y'))
                    Label_7=Widget_label(base_8,/Align_left,uname='Label_7', value="Data Export:")
                    But_4 = CW_PDMENU(base_8, uname='But_4', ['1\X\selList','0\unified mass list','2\file mass list'], /RETURN_FULL_NAME) 
                    Label_8=Widget_label(base_8, uname='Label_8', value="     unified mass list     ", uvalue='unified mass list')
             But_6 = WIDGET_BUTTON(base_2, uname='But_6', event_pro='export', value='       Export        ')
              base_10=widget_base(base_2,/row,uname='base_10',xsize=getpar('base_10x'),ysize=getpar('base_10y'))
                      Label_9=Widget_label(base_10,/Align_left, uname='Label_9', value="File number (first - last):")
                      Text_Ind1 = WIDGET_TEXT(base_10, uname='Text_Ind1', VALUE='0',/EDITABLE,xsize=getpar('Text_Ind1x'), scr_xsize=getpar('Text_Ind1x2'))
                      Text_Ind2 = WIDGET_TEXT(base_10, uname='Text_Ind2', VALUE='0',/EDITABLE,xsize=getpar('Text_Ind2x'), scr_xsize=getpar('Text_Ind2x2'))
                      But_MB1 = WIDGET_BUTTON(base_2, uname='But_MB1',event_pro='createAvgSpectra', value='Mass calibration + avgerage Spectra')
              But_9 = WIDGET_BUTTON(base_2, uname='But_9',event_pro='ExtendedProc', value='       Extended processing      ')

              base_MB_10=widget_base(base_2,/row,uname='base_MB_10',xsize=getpar('base_10x'),ysize=getpar('base_10y'))
              Label_9=Widget_label(base_MB_10,/Align_left, uname='Label_MB_1', value="average time [s]:")
              Text_Ind_MB1 = WIDGET_TEXT(base_MB_10, uname='averageTime', VALUE='60',/EDITABLE,xsize=getpar('Text_Ind1x'), scr_xsize=getpar('Text_Ind1x2'))

             base_12=widget_base(base_1,/column,uname='base_12',xsize=getpar('base_12x'),ysize=getpar('base_12y'))
                     Text_log = WIDGET_TEXT(base_12, uname="Text_log", VALUE=[log],/ALL_EVENTS, /wrap ,/scroll, YSIZE = getpar('Text_logy'))
                     Text_info = WIDGET_TEXT(base_12, uname="Text_info", VALUE=[info],/ALL_EVENTS, /wrap ,/scroll, YSIZE = getpar('Text_infoy'))
  WIDGET_CONTROL, base_1, /REALIZE
  XMANAGER, 'PTRwid', base_1, /NO_BLOCK 
END



pro export, event
; called by button 'export'
;this routine retrieves and exports cps and ppb from the specified files (adjust index i in the main loop)
;UNIFIED MASS LIST MUST BE AVAILABLE
; if you want to re-calculate a file, remove the corresponding ppb-file from the destination folder
compile_opt idl2
WIDGET_CONTROL, /HOURGLASS 
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), get_uVALUE= MassListType 
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestFolder 
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind1'), get_VALUE=iFile1
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind2'), get_VALUE=iFile2
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), GET_uVALUE=Files
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), get_uVALUE= msss 
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_5'), get_uVALUE= Filepar 
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'label_2'), gET_uVALUE= Names
mist=systime(1)


; if export is called from "export one" pics of the mass spectrum are produced
iFile1=max(long(iFile1))
iFile2=max(long(iFile2))
length=max(size(files,/dimensions))
if(iFile2 gt length-1) then iFile2=length-1
if(iFile2 lt iFile1) then iFile2=iFile1

if(getpar('ExportOne') eq 1) then begin 
      jpeg=1
      iFile2=iFile1
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind2'), SET_VALUE=string(iFile2, format='(I5)')
endif else jpeg=0


ID=''
if (MassListType eq 'unified mass list') then  ID='UL'
if (MassListType eq 'file mass list') then  ID='FL'
  

; clear lost memory
help,/memory
print, 'heap_gc'
heap_gc
help,/memory

 
if (ID eq 'UL') then begin
        massesUL=readcsv(strjoin([DestFolder,'unifiedmasslist.csv']))
        if(massesUL[0] eq -9999) then begin 
              UniMassList, event
              massesUL=readcsv(strjoin([DestFolder,'unifiedmasslist.csv']))
        endif 
endif


;set tol parameter  
tol=0.05
                 
if(iFile2-iFile1 gt 0) then  files=files[iFile1+indgen(iFile2-iFile1+1)] else files=files[iFile1]
if(iFile2-iFile1 gt 0) then  names=names[iFile1+indgen(iFile2-iFile1+1)] else names=names[iFile1]

for i=0,iFile2-iFile1 do begin

    ;    missttii=0
    ;    delvar, Sumspec
    ;    print, [var_exists(sumspec),123]
        help,/memory
        ;check if file exits already
        Name1=names[i]
        exists=file_info(strjoin([DestFolder,'Export/',ID,'/ppb/','ppb',Name1,ID,'.fdt'],'')) & exists=exists.exists
        if (jpeg eq 1) then exists=0  ;i.e. if exportOne is selected the files will be overwritten anyway
        if (exists eq 1)then begin
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), gET_VALUE= log
                log=[strjoin(['FILE:  ', Name1,'',ID]),'File index: '+strtrim(string(i+iFile1,Format='(I4)'),2), 'Export data exist already','------------','',log]
                if(max(size(log,/dimensions)) gt 100) then log=log[0:99]
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE= log
        endif
        if (exists ne 1) then begin      
                NamePar=''
                NameBaseline=''
                exists2=file_info(destfolder+'FileInfo/Time2MassPAR/'+name1+'PAR2.fdt') & exists2=exists2.exists
                if (exists2 eq 1)then NamePar=destfolder+'FileInfo/Time2MassPAR/'+name1+'PAR2.fdt' 
                if (exists2 eq 1)then NameBaseline=destfolder+'FileInfo/Baseline/'+name1+'Baseline.fdt' 
                if (NamePar ne '') then  if (max(readfloat(NamePar)) eq -9999) then NamePar=''
                if (NameBaseline ne '') then   baseline=readfloat(NameBaseline)
                if (NamePar ne '') then begin       
                        par=readfloat(NamePar)
                        a=par[0]
                        t0=par[1]
                        ex=par[2]
                        maxmass=par[3]
                        res=par[8]
                        mode=par[9]
                endif else begin
                        Sumspec=getsumspec(files[i])
                        if(max(sumspec.duration) ne -9999) then begin
                                Filepar=PS1(event,sumspec.sumspec, sumspec.SampInt, sumspec.duration, sumspec.cycles, sumspec.extractions,destfolder,names[i],sumspec.instrument)
                                if(max(Filepar.counts) ne -9999)then  Filepar.counts[where(Filepar.counts gt 0)]=Filepar.counts[where(Filepar.counts gt 0)]/sumspec.duration
                                a=Filepar.a
                                t0=Filepar.t0
                                ex=Filepar.ex
                                maxmass=Filepar.maxmass
                                res=Filepar.resolution
                                mode=strpos('H3O+ NO+ O2+',filepar.mode)
                                baseline=Filepar.baseline
                                makefloat, destfolder+'FileInfo/IonList/'+names[i]+'FL2.fdt', Filepar.masslist 
                                makefloat, destfolder+'FileInfo/Time2MassPAR/'+names[i]+'PAR2.fdt', [Filepar.a,Filepar.t0,Filepar.ex,Filepar.maxmass,sumspec.SampInt,Filepar.a3,Filepar.t03,Filepar.ex3,Filepar.resolution,strpos('H3O+ NO+ O2+',filepar.mode)] 
                                makefloat, destfolder+'FileInfo/PeakShape/'+names[i]+'PeakShape.fdt', Filepar.PeakShape 
                                makefloat, destfolder+'FileInfo/Baseline/'+names[i]+'Baseline.fdt', baseline 
                        endif 
                endelse
                
                
                
                
                if (ID eq 'UL' and var_exists(a) eq 1) then   begin
                        massessUL=massesUL[*,0]
                        exists2=file_info(destfolder+'FileInfo/IonList/'+name1+'FL2.fdt') & exists2=exists2.exists
                        if (exists2 eq 1)then NameMsss=destfolder+'FileInfo/IonList/'+name1+'FL2.fdt' else NameMsss='jjj'
                        if (NameMsss ne '') then msss=readfloat(NameMsss)
                        massessFL=msss[*,0]
                        if (mode eq 0) then begin
                                iUL=where(abs(massessUL-21.0221 ) eq min(abs(massessUL-21.0221 )))
                                iFL=where(abs(massessFL-21.0221 ) eq min(abs(massessFL-21.0221 )))
                                massessUL[iUL[0]]=massessFL[iFL[0]]
                                if(getpar('m38') eq 1) then begin
                                        iUL=where(abs(massessUL-38.0326 ) eq min(abs(massessUL-38.0326 )))
                                        iFL=where(abs(massessFL-38.0326 ) eq min(abs(massessFL-38.0326 )))
                                        massessUL[iUL[0]]=massessFL[iFL[0]]
                                endif else begin
                                        iUL=where(abs(massessUL-39.0327 ) eq min(abs(massessUL-39.0327 )))
                                        iFL=where(abs(massessFL-39.0327 ) eq min(abs(massessFL-39.0327 )))
                                        massessUL[iUL[0]]=massessFL[iFL[0]]
                                endelse
                        endif
                        if (mode eq 5) then begin
                                iUL=where(abs(massessUL-30.994 ) eq min(abs(massessUL-30.994 )))
                                iFL=where(abs(massessFL-30.994 ) eq min(abs(massessFL-30.994 )))
                                massessUL[iUL[0]]=massessFL[iFL[0]]
                                iUL=where(abs(massessUL-47.9966) eq min(abs(massessUL-47.9966 )))
                                iFL=where(abs(massessFL-47.9966 ) eq min(abs(massessFL-47.9966 )))
                                massessUL[iUL[0]]=massessFL[iFL[0]]
                        endif
                        if (mode eq 9) then begin
                                iUL=where(abs(massessUL-33.9935 ) eq min(abs(massessUL-33.9935 )))
                                iFL=where(abs(massessFL-33.9935 ) eq min(abs(massessFL-33.9935 )))
                                massessUL[iUL[0]]=massessFL[iFL[0]]
                        endif
        
                     
                        
                        peakTable=peaktable(massessUL,res)
                endif
                if (ID eq 'FL' and var_exists(a) eq 1) then begin
                        NameMsss=''
                        exists2=file_info(destfolder+'FileInfo/IonList/'+name1+'FL2.fdt') & exists2=exists2.exists
                        if (exists2 eq 1)then NameMsss=destfolder+'FileInfo/IonList/'+name1+'FL2.fdt'
                        if (NameMsss ne '') then msss=readfloat(NameMsss)
                        peakTable=peaktable(msss[*,0],res)
                endif
                
               if(var_exists(a) eq 1) then if(var_exists(maxmass) eq 1) then m_max=min([maxmass-0.3,max(PeakTable[*,0])-0.1]) else m_max=max(PeakTable[*,0])-0.1
                m_min=10.5
                if(NamePar eq '') then missttii=sumspec.duration else missttii=111
                if (missttii eq -9999)then begin
                        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), gET_VALUE= log
                        log=[strjoin(['FILE:  ', Name1,'',ID]),'File index: '+strtrim(string(i+iFile1,Format='(I4)'),2), 'Problem reading SumSpec','------------','',log]
                        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE= log
                endif
                if(var_exists(a) eq 1) then  if (a eq -9999)then begin
                        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), gET_VALUE= log
                        log=[strjoin(['FILE:  ', Name1,'',ID]),'File index: '+strtrim(string(i+iFile1,Format='(I4)'),2), 'Crude cal FAILED','------------','',log]
                        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE= log
                endif
                if(var_exists(a) eq 1) then  if (missttii ne -9999 and a ne -9999)then begin
                    info=['Start data export:','']
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
   
                        EngData=GetEngData(event,files[i])
                        misst=LoadMassRange(Files[i],20.5,20.7,a,t0,ex, SampInt)
                        if ( mean(*misst[0]) eq -9999 or max(engdata.duration) eq -9999)then begin
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), gET_VALUE= log
                                log=[strjoin(['FILE:  ', Name1,'',ID]),'File index: '+strtrim(string(i+iFile1,Format='(I4)'),2), 'NOT EXPORTED:','HD5 READ PROBLEM','',log]
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE= log
                        endif
                        if(max(engdata.duration) gt 0 and mean(*misst[0]) ne -9999) then begin
                                cycles=size(*misst,/dimensions)
                                cycles=min(cycles[1])
                                cycles2=cycles
                                sig=total(*misst,1)
                                while(sig[cycles-1] eq 0) do cycles=cycles-1
                                cyc=indgen(cycles)
                                *misst=0
                                SampInt=EngData.SampInt
                                Extractions=EngData.extractions
                                poiscor=EngData.poiscor
                                engdati=engdata.data
                                engdati=engdati[cyc,*]
                                udrift= engdati[*,min(where(strmatch(engdata.names,'Udrift*') eq 1)) ]
                                pdrift= engdati[*,where(strmatch(engdata.names,'p_drift*') eq 1 or strmatch(engdata.names,'p-Drift*') eq 1)]
                                tdrift= 273.15 +engdati[*,WHERE(strmatch(engdata.names,'Drift_Temp*') eq 1 or strmatch(engdata.names,'T-Drift*') eq 1)]
                                pdrift=pdrift[*,0]
                                udrift=udrift[*,0]            
                                tdrift=tdrift[*,0]
                                makeFloat, strjoin([DestFolder,'Export/EngData/','EngData',Name1,'.fdt'],''), engdati
                                if(getpar('SaveCsv') eq 1) then MakeCsv,strjoin([DestFolder,'Export/EngData/','EngData',Name1,'.csv'],''),engdati
                                makeCsv, strjoin([DestFolder,'Export/EngData/','EngDataNames',Name1,'.csv'],''), transpose(EngData.names)
                               
                                print, 'engineering data loaded and saved; time =  ', systime(1)-mist
                                info=[info,'Engineering data loaded and saved']
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
  
                                ;create a list of 'masses'and 'deltas'which defines the integration boundaries for each mass peak; save a list with mass names
                                PeakTable2= PeakTable[where(PeakTable[*,0] gt m_min),*] & PeakTable2=PeakTable2[where(PeakTable2[*,0] lt m_max),*]
                                masses=(PeakTable2[*,2]+PeakTable2[*,1])/2
                                deltas=PeakTable2[*,2]-PeakTable2[*,1]
                                makeCsv,strjoin([DestFolder,'Export/',ID,'/IonList/','MassIDs_',Name1,ID,'.csv'],''),transpose(PeakTable2[*,0]) 
                                ;prepare parameters for reading HDF5
                                Start =floor(m2t(m_min, a, t0, ex,SampInt)) 
                                if(Start lt 0) then Start=0
                                Width = ceil(m2t(m_max, a, t0, ex,SampInt)- Start)
                                timerow=LINDGEN(Width)+Start
                                split=max(ceil(float(Width)*engdata.cycles/getpar('JunkSize')) )
                               
                                print, split
                                 info=[info,'Importing Raw Data in'+string(split,format='(I3)')+' junk(s)']
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
  
                                
                                leng=floor(float(Width)/(split))
                                ;read junks of the HDF5 raw data, perform integration of peaks and create a cps-file
                                cps=fltarr(1,cycles)
                                for k=0,split-1 do begin
                                         if(k gt 0) then mmi=mma else  mmi=m2t(timerow[0], a, t0, ex,SampInt,/time)
                                         if(timerow[0] eq 0) then mmi=mmi+2*tol
                                         mma=m2t(timerow[leng*(k+1)-1], a, t0, ex,SampInt,/time)
                                        if (max(where(masses gt mmi and masses le mma)) gt -0.5) then begin
                                                mistm=masses[where(masses gt mmi)] 
                                                mistd=deltas[where(masses gt mmi)]
                                                msses=mistm[where(mistm le mma)]
                                                dltas=mistd[where(mistm le mma)]
                                                if(min(msses-dltas/2) gt mmi) then  tollow=2*tol  else tollow=2*tol+mmi-min(msses-dltas/2)
                                                if(max(msses+dltas/2) lt mma) then  tolhig=2*tol  else tolhig=2*tol+max(msses+dltas/2)-mma
                                                Start =floor(m2t(mmi-tollow, a, t0, ex,SampInt)) 
                                                Width = ceil(m2t(mma+tolhig, a, t0, ex,SampInt)- Start)
                                                tmrow=LINDGEN(Width)+Start
                                                data=LoadMassRange(Files[i],mmi-tollow,mma+tolhig,a,t0,ex,SampInt)
                                                if(max(baseline) eq -9999) then baseline1=-9999 else baseline1=baseline[Start+lindgen(Width)]
                                                rsch=string(indgen(split))
                                                ccc=integrate(*data,baseline1, tmrow,msses,dltas,tol,a,t0,ex,fltarr(cycles2)+a,fltarr(cycles2)+t0,fltarr(cycles2)+ex,strjoin([DestFolder,'Export/',ID,'/cps/',Name1,rsch[k]],''),jpeg,SampInt,extractions,poiscor)
                                                ccc=ccc[*,cyc]
                                                cps=[(cps),ccc]
                                                *data=0
                                                info=[info,'Junk'+string(k+1,format='(I3)')+'  imported']
                                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
  
                                        endif
                                endfor
                                print,'data',i+iFile1,'  integrated','    time', systime(1)-mist
                                info=[info,'Data integrated']
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
  
                                cps=cps[1:max(size(masses,/dimensions)),*]
                                cps=cps*engdata.cycles/engdata.duration
                                MakeFloat,strjoin([DestFolder,'Export/',ID,'/cps/','cps',Name1,ID,'.fdt'],''),cps
                              print,'cps.fdt',i+iFile1,'  saved','    time', systime(1)-mist
                               
                                if(getpar('SaveCsv') eq 1) then begin
                                        lis=['',string(PeakTable2[*,0],format='(F9.3)')]
                                        hed=strarr(max(d(lis)))
                                        hed[0]='Integrated signal [counts ner second; cps]; 2nd row: m/z; 1st column: timerow [days since 1.1.2009]'
                                        timrow=string(engdati[*,0],format='(D14.8)')
                                        str=[transpose(timrow),string(cps)]
                                        str=[[hed],[lis],[str]]
                                        MakeCsv,strjoin([DestFolder,'Export/',ID,'/cps/','cps',Name1,ID,'.csv'],''),str
                                endif
                                print,'cps.csv',i+iFile1,'  saved','    time', systime(1)-mist
                                 info=[info,'File saved: cps']
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
  
                                if(getpar('CorrectOverlap') eq 1) then begin
                                        misss=name1
                                        corrcps=overlap(masses,cps,misss,destfolder)
                                        MakeFloat,strjoin([DestFolder,'Export/',ID,'/cps/OCorr/','corrcps',Name1,ID,'.fdt'],''),corrcps
                                        if(getpar('SaveCsv') eq 1) then begin
                                                lis=['',string(PeakTable2[*,0],format='(F9.3)')]
                                                hed=strarr(max(d(lis)))
                                                hed[0]='Integrated signal corrected for overlapping peaks [counts ner second; cps]; 2nd row: m/z; 1st column: timerow [days since 1.1.2009]'
                                                timrow=string(engdati[*,0],format='(D14.8)')
                                                str=[transpose(timrow),string(corrcps)]
                                                str=[[hed],[lis],[str]]
                                                MakeCsv,strjoin([DestFolder,'Export/',ID,'/cps/OCorr/','corrcps',Name1,ID,'.csv'],''),str
                                        endif
                                        print,'corrcps',i+iFile1,'  saved','    time', systime(1)-mist
                                        info=[info,'File saved: corrcps']
                                        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
                                
                                endif
                                ; fix if engineering data and raw TOF data do not contain the same number of cycles
                                l2= max(size(pdrift,/dimensions))
                                if(cycles ne l2) then begin
                                        print, 'WARNING: mismatch length engineering and tof data', l2,cycles 
                                        if(cycles gt l2) then begin
                                                for kkk=1,cycles-l2 do begin
                                                        udrift=[udrift,mean(udrift)]
                                                        pdrift=[pdrift,mean(pdrift)]
                                                        tdrift=[tdrift,mean(tdrift)]
                                                endfor
                                        endif else begin
                                                udrift=udrift[0,cycles-1]
                                                pdrift=pdrift[0,cycles-1]
                                                tdrift=tdrift[0,cycles-1]
                                        endelse
                                endif
                                ; compute and save ppb data
                                
                                Prim=getPrimIons(masses,cps,mode)
                                ppb=calcppb( cps, masses,Prim.A, Prim.B,pdrift,udrift,tdrift)
                                MakeFloat,strjoin([DestFolder,'Export/',ID,'/ppb/','ppb',Name1,ID,'.fdt'],''),ppb
                                if(getpar('SaveCsv') eq 1) then begin
                                        lis=['',string(PeakTable2[*,0],format='(F9.3)')]
                                        hed=strarr(max(d(lis)))
                                        hed[0]='Volume mixing ratio [nmol/mol; ppb]; 2nd row: m/z; 1st column: timerow [days since 1.1.2009]'
                                        timrow=string(engdati[*,0],format='(D14.8)')
                                        str=[transpose(timrow),string(ppb)]
                                        str=[[hed],[lis],[str]]
                                        MakeCsv,strjoin([DestFolder,'Export/',ID,'/ppb/','ppb',Name1,ID,'.csv'],''),str
                                endif
                                info=[info,'File saved: ppb']
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
    
                              
                                if(getpar('CorrectOverlap') eq 1) then begin
                                        corrppb=calcppb( corrcps, masses,Prim.A, Prim.B,pdrift,udrift,tdrift)
                                        MakeFloat,strjoin([DestFolder,'Export/',ID,'/ppb/OCorr/','corrppb',Name1,ID,'.fdt'],''),corrppb
                                        if(getpar('SaveCsv') eq 1) then begin
                                                lis=['',string(PeakTable2[*,0],format='(F9.3)')]
                                                hed=strarr(max(d(lis)))
                                                hed[0]='Volume mixing ratio corrected for overlapping peaks [nmol/mol; ppb]; 2nd row: m/z; 1st column: timerow [days since 1.1.2009]'
                                                timrow=string(engdati[*,0],format='(D14.8)')
                                                str=[transpose(timrow),string(corrppb)]
                                                str=[[hed],[lis],[str]]
                                                MakeCsv,strjoin([DestFolder,'Export/',ID,'/ppb/OCorr/','corrppb',Name1,ID,'.csv'],''),str
                                        endif
                                        info=[info,'File saved: corrppb']
                                        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
                                endif
                                print,'ppb',i+iFile1,'  calculated & saved','    time', systime(1)-mist
                              
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), gET_VALUE= log
                                log=[strjoin(['FILE:  ', Name1,'',ID]),'File index: '+strtrim(string(i+iFile1,Format='(I4)'),2), 'EXPORT successful', 'processing time: '+strtrim(string( systime(1)-mist,Format='(I6)'),2)+'s','------------','',log]
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE= log
                                info=[info,'','Export complete']
                                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
                         
                        endif
                endif
        endif
;print, missttii
;print, var_exists(sumspec)
endfor

print, 'fertig'

end
 
 

PRO ExtendedProc, event
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=path 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), GET_uVALUE=lib 
 
  if(strpos(path,'w_data') lt -0.5) then path='click the right mouse button to select "w_data" directory'
  base = WIDGET_BASE(/column, XSIZE = 210, YSIZE = 96, RESOURCE_NAME = 'Extended Processing')
            buti1 = WIDGET_BUTTON(base,event_pro='ProcPTR', value='Average and Merge Data',uname='Buti1',uvalue=path) 
            buti2 = WIDGET_BUTTON(base,event_pro='IdentPTR', value='Attribute Formulas',uname='Buti2',uvalue=lib) 
            buti3 = WIDGET_BUTTON(base,event_pro='FiltPTR', value='Filter samples',uname='Buti3') 
        WIDGET_CONTROL, base, /REALIZE
  XMANAGER, 'ExtendedProc', base, /NO_BLOCK
 print, buti1
 print, buti2
END


PRO createAvgSpectra, event

  WIDGET_CONTROL, /HOURGLASS
  runtime_strt=systime(1)
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestFolder
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'),  GET_uVALUE=Files
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'label_2'), gET_uVALUE= Names
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), gET_VALUE= log
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), gET_VALUE= info

  Names=Files
  n = size(Names,/DIMENSIONS)
  for i=0,n[0]-1 do begin
    Names[i]=FILE_BASENAME(Files[i])
  endfor
  
  i=0
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind1'), SET_VALUE=string(i, format='(I5)')
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind2'), SET_VALUE=string(i, format='(I5)')

  ;delete data from previous file
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_6'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_7'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_1'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_2'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_3'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_4'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_5'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_6'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_10'), set_uVALUE= -9999
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_12'), set_uVALUE= -9999

numberOfFiles=(size(Files,/DIMENSIONS))[0]-1
; read ToF-parameters a, t0, ex from the first file, do mass correction and baseline for every "new" file not already processed:
  for i=0,numberOfFiles do begin
    Name1=Names[i]
    tmp = file_info(destFolder+name1+'_corrected.hdf5') & tmp=tmp.exists
    
    if (tmp eq 1) then begin
      if (i eq 0) then begin
        file_id = H5F_OPEN(destFolder+name1+'_corrected.hdf5')
        id = H5D_OPEN(file_id, '/SumSpectrum')
        aid = H5A_OPEN_NAME(id,'A')
        a0 = H5A_READ(aid);
        aid = H5A_OPEN_NAME(id,'Ex')
        ex0 = H5A_READ(aid);
        aid = H5A_OPEN_NAME(id,'T0')
        t00 = H5A_READ(aid);
        aid = H5A_OPEN_NAME(id,'SampleInterval')
        sampleInterval = H5A_READ(aid);
        ;aid = H5A_OPEN_NAME(id,'IntegrationTime')
        ;inttime=inttime + H5A_READ(aid);
        H5D_CLOSE, id
        H5A_CLOSE, aid
        H5F_CLOSE, file_id

      endif
    endif else begin
    
    EngData=getengdata(event, files[i])
    runtime_OpenFile=systime(1)-runtime_strt
    if(engdata.duration ne -9999) then begin
      NamePar=''
      exists=file_info(destfolder+'FileInfo/Time2MassPAR/'+name1+'PAR2.fdt') & exists=exists.exists
      exists3=file_info(destfolder+'FileInfo/IonList/'+name1+'FL2.fdt') & exists3=exists3.exists
      if (exists eq 1)then NamePar=destfolder+'FileInfo/Time2MassPAR/'+name1+'PAR2.fdt'
      if (exists3 eq 1)then NameMsss=destfolder+'FileInfo/IonList/'+name1+'FL2.fdt'
      if (NamePar ne '') then   if (max(readfloat(NamePar)) eq -9999) then NamePar=''
      if (NamePar ne '') then begin
        par=readfloat(NamePar)
        a=par[0]
        t0=par[1]
        ex=par[2]
        maxmass=par[3]
        msss=readfloat(NameMsss)
        baseline=readfloat(destfolder+'FileInfo/Baseline/'+name1+'Baseline.fdt')
        PeakShape = readfloat(destfolder+'FileInfo/PeakShape/'+name1+'PeakShape.fdt')
        ResolutionVsMass = readfloat(destfolder+'FileInfo/PeakShape/'+name1+'ResolutionVsMass.fdt')
        Filepar=create_struct('file',files[i],'name1',name1,'index',i,'baseline',baseline,'maxmass', maxmass,'a',a,'t0',t0,'ex',ex,'masslist',msss,'masslistnames',['PeakMax[Da]','StartPeak[Da]','EndPeak[Da]','PeakBroadness','SlopeMax','SlopeMin','PeakHight[counts]','PeakMax[time]','BaseHight[counts]'],'resolution',par[8],'PeakShape',PeakShape,'ResolutionVsMass',ResolutionVsMass)


        Print, 'filepar'
        print, systime(1)-runtime_strt

      endif else begin
        S1=PS1(event,EngData.SumSpec,EngData.SampInt,EngData.duration,EngData.cycles, EngData.extractions, destfolder,Name1,  EngData.instrument)
        ;masslistnames=['PeakMax[Da]','StartPeak[Da]','EndPeak[Da]','Resolution','SlopeMax','SlopeMin','PeakHight[counts]','PeakMax[time]','BaseHight[counts]']
        ;    s1=CREATE_STRUCT('MaxMass',maxMass,'a', a,'t0',t0,'ex',ex,'scorParVar',scorParVar,'a3', a3,'t03',t03,'ex3',ex3,'scor3',scor3,'massnames',massnames,$
        ;    'counts', counts[1:10],'res',res[1:10], 'ErrCode', ErrCode,'masslistnames',masslistnames,'MassList',MassList, $
        ;    'baseline', baselineSM/extr, 'PeakShape',[[x7],[dataX3]],'resolution',reso)
        Filepar=create_struct('file',files[i],'name1',name1,'index',i,S1)
        if(max(Filepar.counts) gt 0) then Filepar.counts[where(Filepar.counts gt 0)]=Filepar.counts[where(Filepar.counts gt 0)]/EngData.duration
      endelse
      if(Filepar.a ne -9999) then begin  ;don't do stuff below if calcrude failed
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_EngDat'), SET_VALUE= EngData.names
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_EngDat'), SET_uVALUE= EngData
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string(Filepar.masslist[*,0],FormaT='(f9.3)')
        info=[info,'',string(Filepar.masslist[*,0],FormaT='(f12.3)')]
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), SET_VALUE= info


        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= Filepar.masslist[*,0]
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_5'), SET_uVALUE= Filepar
        DetectedPeaks=size(Filepar.masslist,/dimensions)
        OutputString=[strjoin(['#',string(i, format='(I5)'),':  ', Name1]),$
          strjoin(['Cycles:        ',string(engdata.cycles)]) ,$
          strjoin(['Duration [min]:   ',string(engdata.duration/60, format='(F8.2)')]) ,$
          strjoin(['MaxSignal:   ',string( max(EngData.SumSpec), format='(E10.2)')]) ,$
          strjoin(['MaxSignal/s:   ',string( max(EngData.SumSpec)/engdata.duration, format='(I10)')]) ,$
          strjoin(['StrtTime [T09]:   ',string(engdata.starttime, format='(F9.3)')]) ,$
          strjoin(['Maxmass [Da]:',string(Filepar.maxmass)]) ,$
          strjoin(['a:                     ',string(Filepar.a)]) , $
          strjoin(['t0:                    ',string(Filepar.t0)]) ,$
          strjoin(['ex:                    ',string(Filepar.ex)]) , $
          strjoin(['Resolution (FWHM):     ',string(Filepar.resolution, Format='(I5)')]) , $
          strjoin(['Detected peaks:', string(max(DetectedPeaks))]),$
          strjoin(['Total processing time: ',string(systime(1)-runtime_strt , format='(F5.1)'),'s']),'------------','']
        if (NamePar ne '') then begin
          log=[OutputString, log]
        endif else begin
          testdat=strjoin([Filepar.massnames[0],string(Filepar.counts[0], format='(I10)'),string(Filepar.res[0], format='(I10)')])
          for ij=1,9 do testdat=[testdat,strjoin([Filepar.massnames[ij],string(Filepar.counts[ij], format='(I10)'),string(Filepar.res[ij], format='(I10)')])]
          log=[OutputString,'Test: mass, [cps], [FWHM]',testdat,'------------','', log ]
          makefloat, destfolder+'FileInfo/IonList/'+name1+'FL2.fdt', Filepar.masslist
          makefloat, destfolder+'FileInfo/Time2MassPAR/'+name1+'PAR2.fdt', [Filepar.a,Filepar.t0,Filepar.ex,filepar.maxmass, EngData.SampInt,Filepar.a3,Filepar.t03,Filepar.ex3,Filepar.resolution,strpos('H3O+ NO+ O2+',filepar.mode)]
          makefloat, destfolder+'FileInfo/PeakShape/'+name1+'PeakShape.fdt', Filepar.PeakShape
          makefloat, destfolder+'FileInfo/PeakShape/'+name1+'ResolutionVsMass.fdt', Filepar.ResolutionVsMass
          makefloat, destfolder+'FileInfo/Baseline/'+name1+'Baseline.fdt', Filepar.baseline
        endelse
      endif else begin ;calcrude failed
        OutputString=[strjoin(['#',string(i, format='(I5)'),':  ', Name1]),$
          'Crude calibration FAILED', '------------','']
        log=[OutputString, log]
      endelse
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE= log
      Print, 'fertig'
      print, systime(1)-runtime_strt

    endif ; read engdata failed

    
  ; get SumSpectrum: (unit: ions per bin per file)
  if (i eq 0) then begin
    s = getSumSpec(files(i))
    tmp = size(s.SumSpec, /DIMENSIONS)
    sumSpecMean = s.SumSpec
    ;sumSpecMin = sumSpecMean
    ;sumSpecMax = sumSpecMean
    sumSpecM = sumSpecMean
    for ii=0,tmp[0]-1 do begin
      sumSpecM[ii]=m2t(ii,Filepar.a, Filepar.t0, Filepar.ex, s.SampInt, /time)
    endfor
    a0 = Filepar.a
    t00 = Filepar.t0
    ex0 = FilePar.ex
    saveSpec, destfolder, Names(i), sumSpecMean, Filepar, s.SampInt, s.Duration
    ;s2 = total(sumSpecMean[m2t(184.5, Filepar.a, Filepar.t0, Filepar.ex, s.SampInt):m2t(184.5, Filepar.a, Filepar.t0, Filepar.ex, s.SampInt)])


  endif else begin
    s = getSumSpec(files(i))
    tmp = size(s.SumSpec, /DIMENSIONS)
    sumSpec = s.SumSpec
    baseLine = sumSpec
    for ii=1,tmp[0]-2 do begin; tmp
      mToFind = m2t(ii,a0,t00,ex0, s.SampInt, /time) ;this mass we're looking for
      bin = m2t(mToFind,Filepar.a, Filepar.t0, Filepar.ex, s.SampInt)
      ;print, bin
      bin0=floor(bin)
      bin1=bin0+1
      if bin1 lt tmp[0]-1 then begin
        z = s.SumSpec[bin0]+(s.SumSpec[bin1]-s.SumSpec[bin0])*(bin-bin0)
        bl = filepar.baseline[bin0]+(filepar.baseline[bin1]-filepar.baseline[bin0])*(bin-bin0)
        ;z = z/s.duration
        ;if z<0 then z=fehler
        ;if (sumSpecMin[ii] gt z) then sumSpecMin[ii]=z 
        ;if (sumSpecMax[ii] lt z) then sumSpecMax[ii]=z
        sumSpec[ii]=z;
        ;sumSpecMean[ii]=sumSpecMean[ii]*(i/(i+1.0))+z*(1.0/(i+1.0))
      endif

      ;m=m2t(ii,Filepar.a, Filepar.t0, Filepar.ex, s.SampInt, /time)
      ;print, m
    endfor  
    saveSpec, destfolder, Names(i),sumSpec,Filepar, s.SampInt, s.Duration
  
 endelse
    
    
    endelse
  
  endfor ;Main loop with files
  

  
  
  
  ToF_p_a = fltarr(numberOfFiles+1)
  ToF_p_ex = ToF_p_a
  ToF_p_t0 = ToF_p_a
  ToF_p_R = ToF_p_a
  ToF_p_SampleInterval = ToF_p_a
  inttime=0
  
  
  for i=0,numberOfFiles do begin
    
    file = destfolder+Names(i)+'_corrected.hdf5'
    file_id = H5F_OPEN(file)
    dataset_id1 = H5D_OPEN(file_id, '/SumSpectrum')
    id = H5A_OPEN_NAME(dataset_id1,'A')
    ToF_p_a(i) = H5A_READ(id)
    id = H5A_OPEN_NAME(dataset_id1,'Ex')
    ToF_p_ex(i) = H5A_READ(id)
    id = H5A_OPEN_NAME(dataset_id1,'T0')
    ToF_p_t0(i) = H5A_READ(id)
    id = H5A_OPEN_NAME(dataset_id1,'SampleInterval')
    ToF_p_sampleInterval(i) = H5A_READ(id)
    
    if (i eq 0) then begin
      dataset_id1 = H5D_OPEN(file_id, '/SumSpectrum')
      sumSpec = H5D_read(dataset_id1)
      sumSpecMin = sumSpec
      sumSpecMax = sumSpec
      dataset_id1 = H5D_OPEN(file_id, '/Baseline')
      BaseLine = H5D_read(dataset_id1)
      dataset_id1 = H5D_OPEN(file_id, '/Peakshape')
      avgPeakShape = H5D_read(dataset_id1)
      n = size(avgPeakShape, /DIMENSIONS)
      ToF_Peakshape = fltarr(n[0],numberOfFiles+1)
      Tof_PeakShape[*,i]=avgPeakShape[*,1]
      dataset_id1 = H5D_OPEN(file_id, '/Resolution')
      avgResolution = H5D_read(dataset_id1)
      n = size(sumSpec, /DIMENSIONS)
      m=round(m2t(n-1,ToF_p_a[i], ToF_p_t0[i], ToF_p_ex[i], ToF_p_sampleInterval[i], /time))
      ToF_ResolutionVsMass = fltarr(m,numberofFiles+1)
      ToF_ResolutionFitResult = fltarr(numberOfFiles+1,3)
      ToF_ResolutionInterp = fltarr(numberOfFiles+1,9999)
      n = 100; max...
      ToF_Resolution = fltarr(n[0],numberOfFiles+1)
      ToF_Resolution_x = fltarr(n[0],numberOfFiles+1)
      n = size(avgResolution, /DIMENSIONS)
      for ii=1,n[0]-1 do begin
        Tof_Resolution[ii,i]=avgResolution[ii,0]
        Tof_Resolution_x[ii,i]=avgResolution[ii,1]
      endfor
      ;avgPeakShapeMin = avgPeakShape
      ;avgPeakShapeMax = avgPeakShape
    endif else begin
      spec = H5D_read(dataset_id1)
      ;sumSpec = sumSpec#(i/(i+1.0))+spec#(1.0/(i+1.0))
      n = size(spec, /DIMENSIONS)
      for ii=0,n[0]-1 do begin
        sumSpec[ii] = sumSpec[ii]*(i/(i+1.0))+spec[ii]*(1.0/(i+1.0))
        if (sumSpecMin[ii] gt spec[ii]) then sumSpecMin[ii]=spec[ii]
        if (sumSpecMax[ii] lt spec[ii]) then sumSpecMax[ii]=spec[ii]
      endfor

      dataset_id1 = H5D_OPEN(file_id, '/Baseline')
      BaseLine = H5D_read(dataset_id1)
      n = size(PeakShape, /DIMENSIONS)
      for ii=0,n[0]-1 do begin
        BaseLine[ii] = BaseLine[ii]*(i/(i+1.0))+BaseLine[ii]*(1.0/(i+1.0))
      endfor


      dataset_id1 = H5D_OPEN(file_id, '/Peakshape')
      PeakShape = H5D_read(dataset_id1)
      n = size(PeakShape, /DIMENSIONS)
      for ii=0,n[0]-1 do begin
        avgPeakShape[ii] = avgPeakShape[ii]*(i/(i+1.0))+PeakShape[ii]*(1.0/(i+1.0))
        ;if (avgPeakShapeMin[ii] gt peakShape[ii]) then avgPeakShapeMin[ii]=peakShape[ii]
        ;if (avgPeakShapeMax[ii] lt peakShape[ii]) then avgPeakShapeMax[ii]=peakShape[ii]
      endfor
      dataset_id1 = H5D_OPEN(file_id, '/Peakshape')
      avgPeakShape = H5D_read(dataset_id1)
      Tof_PeakShape[*,i]=avgPeakShape[*,1]
      dataset_id1 = H5D_OPEN(file_id, '/Resolution')
      avgResolution = H5D_read(dataset_id1)
      n = size(avgResolution, /DIMENSIONS)
      for ii=0,n[0]-1 do begin
        Tof_Resolution[ii,i]=avgResolution[ii,0]
        Tof_Resolution_x[ii,i]=avgResolution[ii,1]
      endfor
    endelse
    dataset_id1 = H5D_OPEN(file_id, '/SumSpectrum')
    id = H5A_OPEN_NAME(dataset_id1,'A')
    ToF_p_a(i) = H5A_READ(id)
    id = H5A_OPEN_NAME(dataset_id1,'Ex')
    ToF_p_ex(i) = H5A_READ(id)
    id = H5A_OPEN_NAME(dataset_id1,'T0')
    ToF_p_t0(i) = H5A_READ(id)

    id = H5A_OPEN_NAME(dataset_id1,'IntegrationTime')
    inttime=inttime+H5A_READ(id)
    id = H5A_OPEN_NAME(dataset_id1,'Resolution')
    tmp = H5A_READ(id)
    ToF_p_R(i)=tmp;

    ; fit reolution function
    A = [30D0, 0.3D0, 3000D0]
    fita = [1,1,1]
    X = ToF_Resolution_x[where(ToF_Resolution[*,i] gt 0),i]
    Y = ToF_Resolution[where(ToF_Resolution[*,i] gt 0),i]
    coefs = LMFIT(X,Y, A, /DOUBLE, $
      FITA = fita, FUNCTION_NAME = 'ResolFitFunc', ITMAX=2000, TOL=1E-6)
    ToF_ResolutionFitResult(i,*)=A
    xxx = findgen(m-1)+1
    nn = size(xxx, /DIMENSIONS)
    for ii=0,nn[0]-1 do begin
      ToF_ResolutionVsMass[ii,i] = A[2]*((alog(A[0]*xxx[ii]))^A[1])
    endfor
    ; interpolate resolution vs mass and define standard deviation:
    X2 = [0, X, 10000];
    Y2 = [Y[0], Y, Y[-1]];
    x3 = findgen(9999)
    y = interpol(Y2,X2,x3)
    Tof_ResolutionInterp[i,*]=y
  endfor
  
  WINDOW, xsize=1250,ysize=700
  DEVICE, DECOMPOSED = 0
  loadct,12
  !P.MULTI = [0,3,1]
  !x.oMARGIN=[8,8]
  !Y.MARGIN = [3, 1]
  
  
  
  n = size(ToF_Resolution, /DIMENSIONS)
  mmax = max(ToF_Resolution_x)
  
  std = stddev(ToF_ResolutionInterp, DIMENSION=1)
  ;std = std/sqrt(numberOfFiles+1)
  ToF_MeanResolutionInterp = mean(ToF_ResolutionInterp, DIMENSION=1)
  x4 = findgen(9999)
  plot, x4, Tof_MeanResolutionInterp, thick=1, ytitle='Resolution (FWHM)', background=-1, color=0, charsize=2, xrange=[0, mmax]
  oplot, x4, Tof_MeanResolutionInterp, thick=2, color=100
  upper = Tof_MeanResolutionInterp+std
  bottom = Tof_MeanResolutionInterp-std
  oplot, x4, upper, color=100
  oplot, x4, bottom, color=100

  n = size(ToF_Resolution, /DIMENSIONS)
  oplot, ToF_Resolution_x[where(ToF_Resolution[*,0] gt 0),0], ToF_Resolution[where(ToF_Resolution[*,0] gt 0),0], color=0
  for ii=0,n[1]-1 do begin
    oplot, ToF_Resolution_x[where(ToF_Resolution[*,ii] gt 0),ii],ToF_Resolution[where(ToF_Resolution[*,ii] gt 0),ii],color=0
  endfor
  
  
  ;for ii=0,n[1]-1 do begin
  ;  A = ToF_ResolutionFitResult(ii,*);
  ;  oplot, findgen(399), A[2]*(alog(A[0]*findgen(399))^A[1]), color=65 
  ;end
  
  
  n = size(ToF_PeakShape, /DIMENSIONS)
  x = findgen(n[0])
  X2 = x
  
  Tof_MeanPeakShape = mean(ToF_Peakshape, DIMENSION=2)

  ; normalize peakshape to 1 FWHM:
  indizes=where(ToF_MeanPeakShape gt 0.5)
  l = min(indizes)
  r = max(indizes)
  xl = X2[l-1] + (X2[l]-X2[l-1]) * (0.5-ToF_MeanPeakShape[l-1])/(ToF_MeanPeakShape[l]-ToF_MeanPeakShape[l-1])
  xr = X2[r+1] + (X2[r]-X2[r+1]) * (0.5-ToF_MeanPeakShape[r+1])/(ToF_MeanPeakshape[r]-ToF_MeanPeakShape[r+1])
  xc = (xr+xl)/2
  Xnew = (X2-xc)/(xr-xl)
  Xnew = (X2-1999)/(xr-xl)
  ;Xnew = Xnew-xc

  plot, Xnew[1800:2200], ToF_Peakshape[1800:2200,0], /YLOG, ytitle='height', background=-1, color=0, charsize=2, yrange=[1e-4,1]
  for ii=1,n[1]-1 do begin
    oplot, Xnew[1800:2200],ToF_PeakShape[1800:2200,ii],color=0
  endfor  
  oplot, Xnew[1800:2200],ToF_MeanPeakShape[1800:2200], thick=2, color=200
  
  ;m_over_z = 371.1
  m_over_z = 21.02

  xmin = round(m2t(m_over_z-0.2,a0,t00,ex0,ToF_p_SampleInterval[0]))
  xmax = round(m2t(m_over_z+0.2,a0,t00,ex0,ToF_p_SampleInterval[0]))
  tmp=size(sumSpec,/dimensions)
  x=lindgen(max(tmp)-1)
  plot, m2t(x[xmin:xmax],a0,t00,ex0,ToF_p_SampleInterval[0]), /YLOG, sumSpecMax[xmin:xmax], thick=1, XTITLE='Th', YTITLE='signal [counts per second per bin]' , color=0, background=-1, charsize=2
  oplot, m2t(x[xmin:xmax],a0,t00,ex0,ToF_p_SampleInterval[0]),  sumSpecMin[xmin:xmax], thick=1, color=0
  oplot, m2t(x[xmin:xmax],a0,t00,ex0,ToF_p_SampleInterval[0]),  sumSpec[xmin:xmax], thick=2, color=0
  oplot, m2t(x[xmin:xmax],a0,t00,ex0,ToF_p_SampleInterval[0]), baseline[xmin:xmax], thick=2, color=65

  ; after all files are processed, plot individual parameters:
  
  WINDOW, 10, xsize=1250,ysize=700
  DEVICE, DECOMPOSED = 0
  loadct,38
  !P.MULTI = [0,1,4]
  !x.oMARGIN=[8,8]
  !Y.MARGIN = [3, 1]

  x = findgen(numberOfFiles+1);

  plot, x, ToF_p_a, xtitle='file number',ytitle='a', background=-1,thick=2, color=0, charsize=3,  yrange=[min(ToF_p_a),max(ToF_p_a)]
  
  plot, x, ToF_p_ex, xtitle='file number',ytitle='exponent', background=-1,thick=2, color=0, charsize=3, yrange=[min(ToF_p_ex),max(ToF_p_ex)]
  ;plot, x, ToF_p_t0, xtitle='file number',ytitle='t0', background=-1,thick=2, color=0, charsize=3, yrange=[min(ToF_p_t0),max(ToF_p_t0)]
  
  ltof = fltarr(numberOfFiles+1)
  for ii=0,n[1]-1 do begin
    ltof[ii]=m2t(200,ToF_p_a[ii],ToF_p_t0[ii],ToF_p_ex[ii],sampleInterval)*sampleInterval * sqrt(2*6000*1.6E-19/(200*1.66E-27))*1000 ;mm
  endfor
   
  plot, x, ltof, xtitle='file number',ytitle='length', background=-1,thick=2, color=0, charsize=3, yrange=[min(ltof),max(ltof)]
  
  
  plot, x, ToF_p_R, xtitle='file number',ytitle='Resolution', background=-1,thick=2, color=0, charsize=3, yrange=[min(ToF_Resolution),max(ToF_Resolution)]




  tmp = getsumspec(files[0])
  peaks=DetectPeaks(SumSpec, ToF_p_a[0], ToF_p_t0[0],ToF_p_ex[0],tmp.SampInt)
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string([3.1416,peaks[*,0]], format='(F8.4)')
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= [3.1416,peaks[*,0]]




  
  ;Filepar=create_struct()
  ;saveSpec, destfolder, 'totalSumSpectrum',
  
  ;saveSumSpec, destFolder, 'totalSumSpectrum',
  
  a = ToF_p_a[0]
  t0 = ToF_p_t0[0]
  ex = ToF_p_ex[0]
  sampInt = ToF_p_sampleInterval[0]
  R = ToF_p_R
  integrationTime = inttime
  ;ToF_MeanPeakShape
  ;Tof_MeanResolutionInterp
  ;std
  ;massAxis = m2t(vvv,a,t0,ex,sampInt)
  
  Filepar=create_struct('a',a,'t0',t0,'ex',ex,'SampleInterval',sampInt,'Resolution',R,'IntegrationTime',integrationTime, $
    'PeakShape',Tof_MeanPeakShape,'PeakShapeX',Xnew,'ResolutionVsMass',ToF_MeanResolutionInterp,'stdResolutionVsMass',std)
  
  saveSumSpecs, destfolder, 'ResultSpectra', Names, Filepar, peaks[*,0], ToF_p_a, ToF_p_t0, Tof_p_ex, sumSpecMin, sumSpecMax
  ;destfolder+Names(i)+'_corrected.hdf5'
; = fehler
  
  
end

function ResolFitFunc, X, A
  ; c * (log(ax))^b
  bx = alog(A[0]*X)
  RETURN,[ [A[2]*(bx^A[1])], [A[1]*A[2]/A[0]*(bx^(A[1]-1))], [A[2]*alog(bx)*bx^A[1]], [bx]^A[1] ]
end


PRO getDataDir, event
 compile_opt idl2
  COMPILE_OPT hidden
  Path=''
  ; Prompt for path to catalog files 
  Path=dialog_pickfile( TITLE='Select directory BBB', /DIRECTORY )
  Path2=path+'w_data/'
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), SET_VALUE= Path2
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Drop_1'), SET_uVALUE= 0
  print, "fileinfo"
  miist=file_info(path2)
  print, miist.directory
  if (miist.directory eq 0) then begin 
          FILE_MKDIR, path2
     ;     FILE_MKDIR, path2+'FL'
     ;     FILE_MKDIR, path2+'temp'
          FILE_MKDIR, path2+'Export'
          FILE_MKDIR, path2+'Export/EngData'
          FILE_MKDIR, path2+'Export/FL'
          FILE_MKDIR, path2+'Export/FL/CPS'
          FILE_MKDIR, path2+'Export/FL/CPS/OCorr'
          FILE_MKDIR, path2+'Export/FL/IonList'
          FILE_MKDIR, path2+'Export/FL/PPB'
          FILE_MKDIR, path2+'Export/FL/PPB/OCorr'
          FILE_MKDIR, path2+'Export/UL'
          FILE_MKDIR, path2+'Export/UL/CPS'
          FILE_MKDIR, path2+'Export/UL/CPS/OCorr'
          FILE_MKDIR, path2+'Export/UL/IonList'
          FILE_MKDIR, path2+'Export/UL/PPB'
          FILE_MKDIR, path2+'Export/UL/PPB/OCorr'
          FILE_MKDIR, path2+'FileInfo'
          FILE_MKDIR, path2+'FileInfo/Baseline'
          FILE_MKDIR, path2+'FileInfo/IonList'
          FILE_MKDIR, path2+'FileInfo/PeakShape'
          FILE_MKDIR, path2+'FileInfo/Time2MassPAR'
          FILE_MKDIR, path2+'AvgData'
          FILE_MKDIR, path2+'temp'
endif
  if (path NE '') then begin
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), SET_VALUE= path
          WIDGET_CONTROL,  WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), SET_VALUE= ''
          Files=Filelist(event,path)
          num=string(indgen(max(size(Files.disp,/dimensions))),format='(I4)')+':  '
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), SET_VALUE= num+Files.disp
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), SET_uVALUE= Files.path
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'label_2'), SET_uVALUE= Files.disp
          mmist=file_info('C:/PTRwid/')
          if (mmist.directory eq 0) then FILE_MKDIR, 'C:/PTRwid/'
          makeCsv, 'C:/PTRwid/LastWidgetSettings.csv', transpose([path,path2,string(0)])
 
 
  endif
END
 
 

pro LoadSet,event
;called by button 'load last settings'
; loads last saved settings
    file='C:/PTRwid/LastWidgetSettings.csv'
    OPENR, lun, file, /GET_LUN
    data = STRARR(FILE_LINES(file))
    READF, lun, data
    Free_Lun, lun
    datadir=Data[0]
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), SET_VALUE= datadir
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), SET_VALUE=Data[1] 
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Drop_1'), set_uVALUE= uint(Data[2])
path2=Data[1]
 miist=file_info(path2)
  print, miist.directory
  if (miist.directory eq 0) then begin 
          FILE_MKDIR, path2
     ;     FILE_MKDIR, path2+'FL'
     ;     FILE_MKDIR, path2+'temp'
          FILE_MKDIR, path2+'Export'
          FILE_MKDIR, path2+'Export/EngData'
          FILE_MKDIR, path2+'Export/FL'
          FILE_MKDIR, path2+'Export/FL/CPS'
          FILE_MKDIR, path2+'Export/FL/CPS/OCorr'
          FILE_MKDIR, path2+'Export/FL/IonList'
          FILE_MKDIR, path2+'Export/FL/PPB'
          FILE_MKDIR, path2+'Export/FL/PPB/OCorr'
          FILE_MKDIR, path2+'Export/UL'
          FILE_MKDIR, path2+'Export/UL/CPS'
          FILE_MKDIR, path2+'Export/UL/CPS/OCorr'
          FILE_MKDIR, path2+'Export/UL/IonList'
          FILE_MKDIR, path2+'Export/UL/PPB'
          FILE_MKDIR, path2+'Export/UL/PPB/OCorr'
          FILE_MKDIR, path2+'FileInfo'
          FILE_MKDIR, path2+'FileInfo/Baseline'
          FILE_MKDIR, path2+'FileInfo/IonList'
          FILE_MKDIR, path2+'FileInfo/PeakShape'
          FILE_MKDIR, path2+'FileInfo/Time2MassPAR'
          FILE_MKDIR, path2+'temp'
 endif
  
  Files=Filelist(event, datadir)
  num=string(indgen(max(size(Files.disp,/dimensions))),format='(I4)')+':  '
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), SET_VALUE= num+Files.disp
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), SET_uVALUE= Files.path
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'label_2'), SET_uVALUE= Files.disp
 
end
 
 
 
 
pro plot1,event
DEVICE, DECOMPOSED = 0 
loadct,31
; color tabble No. 31:
; black 16
; white 239
; red 28
; light red 140
; magenta 31
; purple 43
; blue 3
; light blue 179
; dark blue 50
; cyan 211
; light cyan= 251
; green 176
; light green 224
; dark green 144
; brown 8
; grey 187


  ;called by click on a engineering dataset
  ;plots engineering dataset
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_EngDat'), GET_uVALUE= EngData
  !P.MULTI = [0,1,1] 
  i=event.index
  dim=size(EngData.data,/dimensions)
  mist=indgen(dim[0])
  plot, mist,  EngData.data[*,i], /YNOZERO,  PSYM = 4, XTITLE='Cycles', YTITLE=EngData.names[i] , color=16, background=239
  
  
end


pro plot2,event
compile_opt idl2
  DEVICE, DECOMPOSED = 0 
  loadct,31
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), GET_uVALUE= msslst
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), GET_uVALUE=lib 
  if (max(lib) eq -9999) then  begin 
          lib=masslib(/extended)
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), sET_uVALUE=lib
  endif
  
  
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestDir 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_EngDat'), GET_uVALUE=EngData 
  if(var_exists(EngData) gt 0) then SampInt=EngData.sampint else begin
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), GET_uVALUE=Files
      Sumspec=getsumspec(files[0])
      SampInt=Sumspec.sampint
  endelse
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;   Unified mass list mDa-bin scale  evaluation
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  ; check whether values are from "unified mass list" (integer masses starting at 0) or a masslist from an individual file
  if(min(msslst) eq 1) then begin  ;unified mass list
          i=event.index-1
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_2'), gET_uVALUE= UniList
          masslist=unilist.peakloc
          sigmalist=UniList.PeakWidth
          y=UniList.PeakCountsPERppmBINS
          ysm=smooth(y,5)
          x=UniList.PpmBINscale
          mass=masslist[i]
          sigm=sigmalist[i]
          filter=where(x gt mass-0.15 and x lt mass+0.15)
          x1=x[filter]
          y1=y[filter]
          filter=where(abs(x-mass) eq min(abs(x-mass)))
          filter=lindgen(11)-5+max(filter)
          x2=x[filter]
          y2=y[filter]
          y2sm=ysm[filter]
          if(max(y2)le 10 ) then  begin
                  r=gaussfit(x2,y2, nterms=3,coeff) 
                  str='P3'
          endif else begin 
                  r=gaussfit(x2,y2+0.0001, coeff)
                  str='P6'
          endelse
          if(coeff[0] lt 0 or coeff[0] gt max(y2)*3 or 1e6*abs(coeff[2])/mass lt 5 or  max(y2)le 5 ) then begin
                  r=gaussfit(x2,y2sm, nterms=3,coeff)
                  str='P3, SM5'
          endif    
          sig1='sigma ='+string(1000*abs(coeff[2]),format='(f4.1)')+ ' mDa'
          sig2=' (i.e. '+string(1e6*coeff[2]/mass,format='(I3)')+' ppm)'
          x3=min(x2)+findgen(100)*(max(x2)-min(x2))/100
          z=(x3-coeff[1])/coeff[2]
          coeff=[coeff,0,0,0]
          yy=coeff[0]*exp((-z^2)/2)+coeff[3]+coeff[4]*x3+coeff[5]*x3^2
          !P.MULTI = [0,1,1]   
          loadct,38
          plot,x1,y1,psym=4,  XTITLE='bin mass scale [Da] (bin size: max of 8 ppm or 1mDa)', YTITLE='# of Files with peak in bin' , color=0, background=-1, charsize=1.5, charthick=2,thick=3, yrange=[0,max(y1)*1.2]
          loadct,31
          oplot,[mass-coeff[2],mass-coeff[2]],[max(y1)*1.05,max(y1)*1.2],thick=1,color=28
          oplot,[mass+coeff[2],mass+coeff[2]],[max(y1)*1.05,max(y1)*1.2],thick=1,color=28
          oplot,[mass-2*coeff[2],mass-2*coeff[2]],[max(y1)*1.05,max(y1)*1.2],thick=1,color=51
          oplot,[mass+2*coeff[2],mass+2*coeff[2]],[max(y1)*1.05,max(y1)*1.2],thick=1,color=51
          oplot, x1, y1, thick=3, color=16
          oplot, x3,yy,color=28, thick=4 ;gaussfit
          oplot, x1, y1, thick=3, color=16,psym=4
          oplot,x2,y2,psym=4, color=28, thick =3
          xyouts,mean(x2),max(y2)+max(y1)*0.025,str,color=24, charsize=1.5,charthick=2
          xyouts, mass-0.15,max(y1)*1.11,strjoin(['m/z:',string(mass,Format='(F8.3)')],'   '),color=16,charthick=2, charsize=1.5
          match=match(mass,lib)
          xyouts, mass-0.15,max(y1)*1.03,'closest matches, dm [mDa]:',color=16,charthick=2, charsize=1.5
          for r=0,8 do begin
                mis=match(mass+match.devi[r]/1000,lib)
                kleur=16
                if (abs(match.devi[r])/1000 lt 2*coeff[2]) then kleur=51
                if (abs(match.devi[r])/1000 lt coeff[2]) then kleur=28
                xyouts, mass-0.15,max(y1)*(0.98-0.04*r),strjoin([string(match.devi[r],format='(F6.1)'),mis.formula],'  '),color=kleur,charthick=2, charsize=1.2
                oplot, [mass+match.devi[r]/1000,mass+match.devi[r]/1000],[(max(y1)*0.97),(max(y1)*1.03)]/0.9,color=kleur,thick=2
          endfor
          xyouts, mass-0.15,max(y1)*0.60,sig1,charthick=2,color=28, charsize=1.5
          xyouts, mass-0.15,max(y1)*0.56,sig2,charthick=2,color=28, charsize=1.5
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= ''
  
  endif
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;   END
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;   PeakCountPERmDa evaluation
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  if(min(msslst) eq 0) then begin  ;PeakCountPERmDa evaluation
          ;plot for unified mass list
          i=event.index
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_2'), gET_uVALUE= UniList
          y=UniList.PeakCountsPERppmBINS
          x=UniList.PpmBINscale
          ysm=smooth(y,5)
          dysm=deriv(x,ysm)  
          filter=where(x gt i-0.4 and x lt i+0.6)
          if(min(filter) gt -0.5) then begin 
                  x1=x[filter]
                  y1=y[filter] ;raw data 'PeakCountsPERmDA'
                  ysm=ysm[filter]
                  dysm=dysm[filter]
                  filter=where((UniList.PeakLoc-min(x1)lt 1) and (UniList.PeakLoc-min(x1) gt 0))
                  if(min(filter) gt -0.5) then begin 
                          xpeaks=UniList.PeakLoc[filter] 
                          xpeaksDer=UniList.PeakLocDer[filter] 
                          sigma=UniList.PeakWidth[filter] 
                          ymax=UniList.PeakHeight[filter] 
                  endif else begin 
                          xpeaks=0
                          sigma=0
                          ymax=0
                  endelse
                  fwhm=sigma*2*SQRT(2*ALOG(2))
         
               
                  print, 'mass   (Da)', xpeaks
                  print, 'fwhm   (mDa)',  fwhm*1000
                  print,'ymax (#files)',  ymax
                  print, 'accur. (ppm)',1E6*sigma/xpeaks
                  print, 'area (#files)',ymax*sigma*1000*2*SQRT(2*ALOG(2))
                 
                 info=['Peaks added to UnigfiesMassList:','',strjoin(['mass   (Da)', string(xpeaks,format='(F9.3)')]),strjoin(['fwhm   (mDa)', string(fwhm*1000,format='(F9.1)')]),strjoin(['ymax (#files)', string(ymax,format='(I9)')]),strjoin(['accur. (ppm)', string(1E6*sigma/xpeaks,format='(I9)')]),strjoin(['area (#files)', string(ymax*sigma*1000*2*SQRT(2*ALOG(2)),format='(I9)')])]
      
      
      
                   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
  
                  !P.MULTI = [0,1,1] 
                  len=max(size(y1,/dimensions))
                  if( max(y1[0:floor(len/3)]) lt 3) then inXr=floor(len/3)+1 else inXr=0
                  if( max(y1[floor(2*len/3):len-1]) lt 3) then inXr2=floor(2*len/3)+1 else inXr2=len-1
                  loadct,38
                  plot, x1,y1,yrange=[0,max([20,2*y1])],xrange=[x1[inXr],x1[inXr2]],title=total(y1), color=0, background=-1, charsize=2, charthick=3, ytitle='# of peak detections in mDa-bin', xtitle='m/z'
                  loadct,31
                  if(max(y1) lt 80) then for i=1,floor(max([20,2*y1])) do oplot,[min(x1),max(x1)],[i,i], color=187
                  oplot, x1,y1, color=16 ;black
                  oplot, x1,dysm/300+max([15,y1*1.4]),color=28 ;red
                  oplot, x1,ysm+max([15,y1*1.4]),color=50  ;blue
                  oplot, x1,y1,thick=3, color=16
                  mist=size(xpeaks,/dimensions)
                  for i=0,mist[0]-1 do begin
                          oplot, [xpeaks[i],xpeaks[i]],[0,abs(ymax[i])],color=211,thick=3 ;cyan
                          oplot, [xpeaks[i],xpeaks[i]],[0,ymax[i]],color=28,thick=3  ;red
                          oplot, [xpeaks[i]-fwhm[i]/2,xpeaks[i]+fwhm[i]/2],[ymax[i]/2,ymax[i]/2],color=28,thick=3
                          PPP=max(where(abs(x1-xpeaks[i]) eq min(abs(x1-xpeaks[i])))) 
                          indis=lindgen(11)+PPP-5
                          xi=x1[indis]
                          r=gaussfit(xi,y1[indis], coeff)
                          z=(xi-coeff[1])/coeff[2]
                          yy=coeff[0]*exp((-z^2)/2)+coeff[3]+coeff[4]*xi+coeff[5]*xi^2
                          oplot, xi,yy,color=3, thick=3
                          oplot,[coeff[1]-coeff[2],coeff[1]+coeff[2]],[0,0],color=3,thick=3
                          xyouts,coeff[1],max(y1[indis])+1*max([20,2*y1])/40,string(1000*coeff[2]*2*SQRT(2*ALOG(2)),format='(f4.1)'),charsize=1.2,charthick=1.5,color=3
                          if(max(y1[indis])le 10 ) then begin
                                  r=gaussfit(xi,y1[indis], nterms=3,coeff)
                                  z=(xi-coeff[1])/coeff[2]
                                  yy=coeff[0]*exp((-z^2)/2);+coeff[3]+coeff[4]*xi+coeff[5]*xi^2
                                  oplot, xi,yy,color=180, thick=3
                                  xyouts,coeff[1],max(y1[indis])+2*max([20,2*y1])/40,string(1000*coeff[2]*2*SQRT(2*ALOG(2)),format='(f4.1)'),charsize=1.2,charthick=1.5,color=180
                          endif
                          if(coeff[0] lt 0 or coeff[0] gt max(y1[indis])*3 or max(y1[indis])le 5 ) then begin 
                                  r=gaussfit(xi,ysm[indis], nterms=3,coeff)
                                  z=(xi-coeff[1])/coeff[2]
                                  yy=coeff[0]*exp((-z^2)/2)
                                  oplot, xi,yy,color=28, thick=3
                                  xyouts,coeff[1],max(y1[indis])+3*max([20,2*y1])/40,string(1000*coeff[2]*2*SQRT(2*ALOG(2)),format='(f4.1)'),charsize=1.2,charthick=1.5,color=28
                          endif
                  endfor
          endif   else plot, [i,i],[i,i]   
  endif
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;   END
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;   plot for masslist from individual file ---MB
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 if((min(msslst)-3.1416) le 0.01 ) then begin ;if first mass is pi then this is a sum spectrum...

    print, 'Martin was here'
    WINDOW, 0, xsize=1250,ysize=700

    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_5'), GET_uVALUE= Filepar
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestFolder
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_EngDat'), GET_uVALUE= Engdata
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Drop_1'), Get_uVALUE= drop1
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), get_uVALUE= MassListType
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), get_uVALUE= masses

    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'averageTime'), GET_VALUE=averageTime
    i=event.index
    minMass = masses[i]-1
    maxMass = masses[i]+1


    file_id = H5F_OPEN(destFolder+'ResultSpectra.hdf5')

    id = H5D_OPEN(file_id, '/SumSpecs')
    aid = H5A_OPEN_NAME(id,'SampleInterval')
    sampleInterval = H5A_READ(aid);
    aid = H5A_OPEN_NAME(id,'A')
    a = H5A_READ(aid);
    aid = H5A_OPEN_NAME(id,'T0')
    t0 = H5A_READ(aid);
    aid = H5A_OPEN_NAME(id,'Ex')
    ex = H5A_READ(aid);


    fileid=file_id
    dataset_id1 = H5D_OPEN(fileid, '/SumSpecs')
    dataspace_id1 = H5D_GET_SPACE(dataset_id1)
    Dims=H5S_GET_SIMPLE_EXTENT_DIMS(Dataspace_id1)

    Start =floor(m2t(minMass,a,t0,ex,SampInt))
    Width = ceil(m2t(maxMass,a,t0,ex,SampInt)- Start)
    mist2=start+width

    if (max(size(Dims,/dimensions)) ge 2) then begin
      ddd=lonarr(size(Dims,/dimensions)-1)
      eee=ddd
      for i=1,max(size(Dims,/dimensions))-1 do eee[i-1]=Dims[i]
      start2 = [Start, ddd]
      count = [Width,eee]
    endif else begin
      start2 = [Start]
      count = [Width]
    endelse

    H5S_SELECT_HYPERSLAB, dataspace_id1, start2, count,  /RESET
    memory_space_id1 = H5S_CREATE_SIMPLE(count)

    Data=rearrange(H5D_READ(dataset_id1, FILE_SPACE=dataspace_id1, MEMORY_SPACE=memory_space_id1)) ; Data

    dataset_id1 = H5D_OPEN(fileid, '/MassAxis')
    dataspace_id1 = H5D_GET_SPACE(dataset_id1)
    H5S_SELECT_HYPERSLAB, dataspace_id1, start, width,  /RESET
    memory_space_id1 = H5S_CREATE_SIMPLE(width)
    X=rearrange(H5D_READ(dataset_id1, FILE_SPACE=dataspace_id1, MEMORY_SPACE=memory_space_id1)) ; Data

    dataset_id1 = H5D_OPEN(fileid, '/Resolution')
    Resolution = H5D_read(dataset_id1)
    dataset_id1 = H5D_OPEN(fileid, '/Peakshape')
    PeakShape = H5D_read(dataset_id1)
    dataset_id1 = H5D_OPEN(fileid, '/PeakshapeX')
    PeakShapeX = H5D_read(dataset_id1)
    dataset_id1 = H5D_OPEN(fileid, '/MassList')
    MassList = H5D_read(dataset_id1)
    peakShapeCum = total(PeakShape, /CUMULATIVE)
    peakShape=peakShape/max(peakShapeCum)
    print, total(peakShape)    
    dataset_id1 = H5D_OPEN(fileid, '/AvgBaseline')
    dataspace_id1 = H5D_GET_SPACE(dataset_id1)
    H5S_SELECT_HYPERSLAB, dataspace_id1, start, width,  /RESET
    memory_space_id1 = H5S_CREATE_SIMPLE(width)
    avgBaseline=rearrange(H5D_READ(dataset_id1, FILE_SPACE=dataspace_id1, MEMORY_SPACE=memory_space_id1)) ; Data

    dataset_id1 = H5D_OPEN(fileid, '/AvgSpectrum')
    dataspace_id1 = H5D_GET_SPACE(dataset_id1)
    H5S_SELECT_HYPERSLAB, dataspace_id1, start, width,  /RESET
    memory_space_id1 = H5S_CREATE_SIMPLE(width)
    avgSpectrum=rearrange(H5D_READ(dataset_id1, FILE_SPACE=dataspace_id1, MEMORY_SPACE=memory_space_id1)) ; Data

    dataset_id1 = H5D_OPEN(fileid, '/SumSpecMax')
    dataspace_id1 = H5D_GET_SPACE(dataset_id1)
    H5S_SELECT_HYPERSLAB, dataspace_id1, start, width,  /RESET
    memory_space_id1 = H5S_CREATE_SIMPLE(width)
    maxSpectrum=rearrange(H5D_READ(dataset_id1, FILE_SPACE=dataspace_id1, MEMORY_SPACE=memory_space_id1)) ; Data

    dataset_id1 = H5D_OPEN(fileid, '/SumSpecMin')
    dataspace_id1 = H5D_GET_SPACE(dataset_id1)
    H5S_SELECT_HYPERSLAB, dataspace_id1, start, width,  /RESET
    memory_space_id1 = H5S_CREATE_SIMPLE(width)
    minSpectrum=rearrange(H5D_READ(dataset_id1, FILE_SPACE=dataspace_id1, MEMORY_SPACE=memory_space_id1)) ; Data



    H5S_CLOSE, memory_space_id1
    H5d_CLOSE, dataset_id1
    H5S_CLOSE, dataspace_id1
    H5f_CLOSE, fileid


    localResolution = resolution[round(masses[i])]
    peaksInvolved = where(MassList gt minMass and MassList lt maxMass)
    n = size(peaksInvolved, /DIMENSIONS)
    m = size(X, /DIMENSIONS)
    peaks = make_array(n[0],m[0],/FLOAT, Value=0)
    for i=0,n[0]-1 do begin
      mass = MassList[peaksInvolved[i]]
      for k=0,m[0]-1 do begin
        dx=(X[k]-mass)*localResolution/mass
        if (dx gt min(peakShapeX)) and (dx lt max(peakShapeX)) then begin
          indx = value_locate(PeakShapeX,dx)
          peaks[i,k]=PeakShape[indx]
        endif
 
      endfor
    endfor
    
    
 peaksum=findgen(n[0])
 ; normalize Peakshape:
    for i=0,n[0]-1 do begin
      peaksum[i]=total(peaks[i,*])
      for jjj=0,m[0]-1 do begin
        peaks[i,jjj]=peaks[i,jjj]/peaksum[i]
      endfor
    endfor
 
    ; solve LGL:
    A = make_array(n[0],n[0],/FLOAT, Value=0)
    S = make_array(n[0],/FLOAT, Value=0)

    for i=0,n[0]-1 do begin
      c = total(peaks[i,*], /CUMULATIVE)
      c=c/max(c)
      bins = where(c gt 0.1 and c lt 0.9)
      bin1 = min(bins)
      bin2 = max(bins)
      S[i] = total(AvgSpectrum[bin1:bin2])-total(AvgBaseline[bin1:bin2])
      for j=0,n[0]-1 do begin
        A[i,j]=total(peaks[j,bin1:bin2])
      endfor
    endfor

    Ai = invert(A)
    coeffs = Ai#S
    

    i=event.index

          Intlist=peaktable(masses,resolution[round(masses[i])])
          IntStart=Intlist[i,1]
          IntEnd=Intlist[i,2]
          start=IntStart
          Pcenter=masses[i]
          Pstart=Intstart
          Pend=IntEnd



    derivData=deriv(Data)
    
    mist=size(data,/dimensions)
    xmin=round(mist[0]*0.25)
    xmax=round(mist[0]*0.75)
    !P.MULTI = [0,2,2]
    loadct,38
    !x.oMARGIN=[0,0] 
    plot, x[xmin:xmax],  avgSpectrum[xmin:xmax], /YNOZERO, xstyle=1,ystyle=1,  XTITLE='Da', YTITLE='signal' , color=0, thick=2, background=-1,yrange=[0,max(maxSpectrum[xmin:xmax])+max(maxSpectrum[xmin:xmax])/10], charsize=1.5
    loadct,31
    oplot, x[xmin:xmax],  minSpectrum[xmin:xmax], color=3
    oplot, x[xmin:xmax],  maxSpectrum[xmin:xmax], color=3

    ;oplot, [intstart,intstart],[min(data[xmin:xmax]),max(data[xmin:xmax])],color=28
    ;oplot, [intend,intend],[min(data[xmin:xmax]),max(data[xmin:xmax])],color=28
    ;oplot, m2t(x[xmin:xmax],a,t0,ex,SampInt),  baseline[xmin:xmax], color=28, thick=2
    ;oplot, m2t(x[xmin:xmax],a,t0,ex,SampInt),  avgSpectrum[xmin:xmax], color=35, thick=2
    ;oplot, [pstart,pend],[max(data[xmin:xmax]),max(data[xmin:xmax])],color=35
    ;oplot, [pstart,pend],[min(data[xmin:xmax]),min(data[xmin:xmax])],color=35, thick=3
    oplot, [Pcenter,Pcenter],[max(data[xmin:xmax])-max(data[xmin:xmax])/20,max(data[xmin:xmax])+max(data[xmin:xmax])/20],color=35
    oplot, [Pcenter,Pcenter],[min(data[xmin:xmax]),min(data[xmin:xmax])+max(data[xmin:xmax])/20],color=35, thick=3
    ;xgauss= m2t(x[m2t(pstart,a,t0,ex,SampInt): m2t(pend,a,t0,ex,SampInt)],a,t0,ex,SampInt)
    ;ygauss=data2[m2t(pstart,a,t0,ex,SampInt): m2t(pend,a,t0,ex,SampInt)]
    ;weights=replicate(1.0, N_ELEMENTS(ygauss))
    ;AA=[max(ygauss),Pcenter,Pcenter/(4000* 2*SQRT(2*ALOG(2)))]
    ;xyouts, m2t(x[xmin],a,t0,ex,SampInt)+0.025,max(data[xmin:xmax]),t09str(EngData.starttime),color=0, charsize=1.1,charthick=2
    xyouts, x[xmin]+0.025,max(maxSpectrum[xmin:xmax])*0.9,'m/z:'+string(masses[i],Format='(F8.3)'),color=0, charsize=1.1,charthick=2
    match=match(masses[i],lib)
    xyouts, x[xmin]+0.025,max(maxSpectrum[xmin:xmax])*1,match.formula,color=28, charsize=1.1,charthick=2
    xyouts, x[xmin]+0.025,max(maxSpectrum[xmin:xmax])*0.8,'Closest matches, dm [mDa]:',color=0, charsize=1.1,charthick=1
    for r=0,8 do if (abs(match.devi[r]) gt 3 or r ne 4) then begin
      mis=match(masses[i]+match.devi[r]/1000,lib)
      xyouts, x[xmin]+0.025,max(maxSpectrum[xmin:xmax])*(0.73-0.07*r),strjoin([string(match.devi[r],format='(F6.1)'),mis.formula],'  '),color=0, charsize=1.1
      oplot, [Pcenter+match.devi[r]/1000,Pcenter+match.devi[r]/1000],[max(maxSpectrum[xmin:xmax])+max(maxSpectrum[xmin:xmax])/30,max(maxSpectrum[xmin:xmax])+max(maxSpectrum[xmin:xmax])/18],color=0
    endif else begin
      xyouts, x[xmin]+0.025,max(maxSpectrum[xmin:xmax])*(0.73-0.07*r),strjoin([string(match.devi[r],format='(F6.1)'),'match'],'  '),color=28, charsize=1.1
      oplot, [Pcenter+match.devi[r]/1000,Pcenter+match.devi[r]/1000],[max(maxSpectrum[xmin:xmax])+max(maxSpectrum[xmin:xmax])/30,max(maxSpectrum[xmin:xmax])+max(maxSpectrum[xmin:xmax])/12],color=28
      oplot, [Pcenter+match.devi[r]/1000,Pcenter+match.devi[r]/1000],[max(maxSpectrum[xmin:xmax])+max(maxSpectrum[xmin:xmax])/12,max(maxSpectrum[xmin:xmax])+max(maxSpectrum[xmin:xmax])/10],color=28,thick=3
    endelse

    if(drop1 gt 0.5) then  begin
      Name1=filepar.name1
      S1=GetFileExport(event, destfolder, Name1)
      if(max([max(S1.cpsFL),max(S1.corrcpsFL),max(S1.cpsUL),max(S1.corrcpsUL)]) eq -9999) then begin
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), set_VALUE= 'file mass list'
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), set_uVALUE= 'file mass list'
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind1'), SET_VALUE=string(filepar.index, format='(I5)')
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind2'), SET_VALUE=string(filepar.index, format='(I5)')
        export, event
        S1=GetFileExport(event, destfolder, Name1)
      endif
      tol=max([0.006,50*pcenter/1e6])
      if(drop1 eq 1) then begin
        if(max(S1.cpsFL) gt -9999) then begin
          filte=where(abs(S1.massFL-pcenter) lt tol)
          if(max(filte) gt -0.5) then datFL=reform(S1.cpsFL[filte,*]) else datFL=-9999
        endif else datFL=-9999
        if(max(S1.corrcpsFL) gt -9999 and max(datFL) ne -9999) then corrdatFL=reform(S1.corrcpsFL[filte,*]) else corrdatFL=-9999
        if(max(S1.cpsUL) gt -9999) then begin
          filte=where(abs(S1.massUL-pcenter) lt tol)
          if(max(filte) gt -0.5) then datUL=reform(S1.cpsUL[filte,*]) else datUL=-9999
        endif else datUL=-9999
        if(max(S1.corrcpsUL) gt -9999 and max(datUL) ne -9999) then corrdatUL=reform(S1.corrcpsUL[filte,*]) else corrdatUL=-9999
        if(max(datFL) ne -9999) then dat=datFL else if(max(datUL) ne -9999) then dat=datUL
        ytit='cps'
      endif
      if(drop1 eq 2) then begin
        if(max(S1.ppbFL) gt -9999) then begin
          filte=where(abs(S1.massFL-pcenter) lt tol)
          if(max(filte) gt -0.5) then datFL=reform(S1.ppbFL[filte,*]) else datFL=-9999
        endif else datFL=-9999
        if(max(S1.corrppbFL) gt -9999 and max(datFL) ne -9999) then corrdatFL=reform(S1.corrppbFL[filte,*]) else corrdatFL=-9999
        if(max(S1.ppbUL) gt -9999) then begin
          filte=where(abs(S1.massUL-pcenter) lt tol)
          if(max(filte) gt -0.5) then datUL=reform(S1.ppbUL[filte,*]) else datUL=-9999
        endif else datUL=-9999
        if(max(S1.corrppbUL) gt -9999 and max(datUL) ne -9999) then corrdatUL=reform(S1.corrppbUL[filte,*]) else corrdatUL=-9999
        if(max(datFL) ne -9999) then dat=datFL else if(max(datUL) ne -9999) then dat=datUL
        ytit='ppb'
      endif

      ; if (var_exists(dat) eq 1) then begin
      if(max(S1.massUL) gt -9999) then begin
        Intlist=peaktable(S1.massUL,filepar.resolution)
        IntStart=Intlist[*,1]
        IntEnd=Intlist[*,2]
        xyouts,m2t(x[xmin],a,t0,ex,SampInt)+0.025,max(data[xmin:xmax])/15,'int. bound. UnifMassList:', color=28,  charthick=1
        xyouts,IntEnd,intarr(max(d(IntEnd)))+max(data[xmin:xmax])/15,strarr(max(d(IntEnd)))+'|', color=208,  charthick=3
        xyouts,IntStart,intarr(max(d(IntStart)))+max(data[xmin:xmax])/15,strarr(max(d(IntStart)))+'|', color=28,  charthick=1
      endif
      if(max(S1.massFL) gt -9999) then begin
        Intlist=peaktable(S1.massFL,filepar.resolution)
        IntStart=Intlist[*,1]
        IntEnd=Intlist[*,2]
        xyouts,m2t(x[xmin],a,t0,ex,SampInt)+0.025,0,'int. bound. FileMassList:', color=28,  charthick=1
        xyouts,IntEnd,intarr(max(d(IntEnd)))+0,strarr(max(d(IntEnd)))+'|', color=208,  charthick=3
        xyouts,IntStart,intarr(max(d(IntStart)))+0,strarr(max(d(IntStart)))+'|', color=28,  charthick=1
      endif

      if (var_exists(dat) eq 1) then begin


        loadct,38

        plot, dat, /YNOZERO  ,  XTITLE='cycles',YTITLE=ytit , yrange=range(dat[where(dat ne -9999)]), color=0, background=-1, thick=2, charsize=1.5
        if(max(corrdatUL) ne -9999) then oplot, corrdatUL, color=100,  thick=2
        if(max(datUL) ne -9999) then oplot, datUL, color=50,  thick=2
        if(max(corrdatFL) ne -9999) then oplot, corrdatFL, color=90,  thick=2
        if(max(datFL) ne -9999) then oplot, datFL, color=40,  thick=2
        if(max(datFL) ne -9999) then xyouts,0.91,0.44,'FL', color=40,/normal,  charthick=2
        if(max(corrdatFL) ne -9999) then xyouts,0.91,0.415,'corr FL', color=90,/normal,  charthick=2
        if(max(datUL) ne -9999) then xyouts,0.91,0.39,'UL', color=50,/normal,  charthick=2
        if(max(corrdatUL) ne -9999) then xyouts,0.91,0.365,'corr UL', color=100,/normal,  charthick=2
        makecsv,DestDir+'temp/last_cps_row.csv' ,dat
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= ['Data (no overlap correction):', reform(string(dat[0,*]))]


      endif else plot, [0,0],[0,0]
    endif else begin ; print derivdata
      loadct,38
      plot, x[xmin:xmax],  avgSpectrum[xmin:xmax], /YLOG, /YNOZERO, xstyle=1,ystyle=1,  XTITLE='Da', YTITLE='signal' , color=0, thick=2, background=-1,yrange=[min(minSpectrum[xmin:xmax])*0.9,max(maxSpectrum[xmin:xmax])+max(maxSpectrum[xmin:xmax])/10], charsize=1.5
      loadct,31
      ;oplot, x[xmin:xmax],  minSpectrum[xmin:xmax], color=3
      ;oplot, x[xmin:xmax],  maxSpectrum[xmin:xmax], color=3
      oplot, x[xmin:xmax],  avgbaseline[xmin:xmax], color=28, thick=2
      ;oplot, x[xmin:xmax],  data[xmin:xmax], color=35, thick=2
      oplot, [Pcenter,Pcenter], [min(data[xmin:xmax]),max(data[xmin:xmax])], color=28
      
      n = size(peaksInvolved, /DIMENSIONS)
      for i=0,n[0]-1 do begin
      ;  oplot, x[xmin:xmax],Peaks[i,xmin:xmax]*coeffs[i], color=144
      endfor

      sumFit = Peaks[0,*]*coeffs[0]
      m = size(sumFit, /DIMENSIONS)
      n = size(coeffs, /DIMENSIONS)
      for i=0,m[1]-1 do begin
        for j=1,n[0]-1 do begin
          sumFit[i]=sumFit[i]+Peaks[j,i]*coeffs[j]
        endfor
      endfor
      oplot, x[xmin:xmax],sumFit[xmin:xmax]+AvgBaseline[xmin:xmax], color=28, thick=2


      
      
      ;      oplot,m2t([x[xmin],x[xmax]],a,t0,ex,SampInt),[0,0],color=0
      ;      oplot, [Pcenter,Pcenter],[-max(derivdata[xmin:xmax])/10, max(derivdata[xmin:xmax])/10],color=28, thick=3
      ;      oplot, [pstart,pend],[0,0],color=28, thick=3
    endelse
   
   ;y=fehler
    
 endif
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;   plot for masslist from individual file
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  
  if(min(msslst) gt 3.5) then begin  ;plot for masslist from individual file
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_5'), GET_uVALUE= Filepar
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestFolder 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_EngDat'), GET_uVALUE= Engdata
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Drop_1'), Get_uVALUE= drop1
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), get_uVALUE= MassListType 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), get_uVALUE= masses 
          i=event.index
          a=filepar.a
          t0=filepar.t0
          ex=filepar.ex
          File=EngData.FileName
          mass=masses[i]
          Intlist=peaktable(masses,filepar.resolution)
          IntStart=Intlist[i,1]
          IntEnd=Intlist[i,2]
          if (MassListType eq 'file mass list')then Pstart=Filepar.masslist[i,1] else Pstart=IntStart
          Pcenter=mass
          if (MassListType eq 'file mass list')then Pend=Filepar.masslist[i,2] else Pend=IntEnd
          baseline=filepar.baseline*engdata.cycles*engdata.extractions 
          data=EngData.SumSpec
     
     
     y=fehler
     
     
     
     
     
    
     
     
     
     
     
     
     faktor=floor(1e-10/sampint)
     if(faktor eq 0) then faktor=1
     
          savgolFilter = SAVGOL(9*faktor, 9*faktor, 0, floor(10/faktor))
          dataX0=CONVOL(Data, savgolFilter, /EDGE_TRUNCATE)   
          derdataX0=CONVOL(deriv(dataX0), savgolFilter, /EDGE_TRUNCATE)   
          savgolFilter = SAVGOL(9*faktor, 9*faktor, 0, floor(6/faktor))
          dataX1=CONVOL(Data, savgolFilter, /EDGE_TRUNCATE)
          derdataX1=CONVOL(deriv(dataX1), savgolFilter, /EDGE_TRUNCATE)   
          savgolFilter = SAVGOL(14*faktor, 14*faktor, 0,  floor(6/faktor))
          dataX2=CONVOL(Data, savgolFilter, /EDGE_TRUNCATE)
          derdataX2=CONVOL(deriv(dataX2), savgolFilter, /EDGE_TRUNCATE)   
          savgolFilter = SAVGOL(20*faktor, 20*faktor, 0,  floor(6/faktor))
          dataX3=CONVOL(Data, savgolFilter, /EDGE_TRUNCATE)
          derdataX3=CONVOL(deriv(dataX3), savgolFilter, /EDGE_TRUNCATE)   
          mist=size(data,/dimensions) 
          data2=data
          ; ~up to m120 use dataX1, between ~120 and ~290 use dataX2, above ~290 use dataX3
          Thresh1=m2t(64.6,a,t0,ex,SampInt)
          Thresh2=m2t(144.6,a,t0,ex,SampInt)
          Thresh3=m2t(324.6,a,t0,ex,SampInt)
          if(mist gt Thresh1) then data2[0:Thresh1-1]=dataX0[0:Thresh1-1] else data2=dataX0
          if(mist gt Thresh2) then data2[Thresh1:Thresh2-1]=dataX1[Thresh1:Thresh2-1] else data2[Thresh1:*]=dataX1[Thresh1:*]
          if(mist gt Thresh3) then begin
                  data2[Thresh2:Thresh3-1]=dataX2[Thresh2:Thresh3-1] 
                  data2[Thresh3:mist-1]=dataX3[Thresh3:mist-1]
          endif else data2[Thresh2:mist-1]=dataX2[Thresh2:mist-1]
          derivData=deriv(Data2)
          if(mist gt Thresh1) then derivData[0:Thresh1-1]=derdataX0[0:Thresh1-1] else data2=derdataX0
          if(mist gt Thresh2) then derivData[Thresh1:Thresh2-1]=derdataX1[Thresh1:Thresh2-1] else derivData[Thresh1:*]=derdataX1[Thresh1:*]
          if(mist gt Thresh3) then begin
                  derivData[Thresh2:Thresh3-1]=derdataX2[Thresh2:Thresh3-1] 
                  derivData[Thresh3:mist-1]=derdataX3[Thresh3:mist-1]
          endif else derivData[Thresh2:mist-1]=derdataX2[Thresh2:mist-1]
         
         
         
         
         
         
         
         
         
         
  
         
         
         
         
         
         
         
         
         
         
         
         
         
         
          mist=size(data,/dimensions)
          x=lindgen(max(mist)-1)
          xmin=round(m2t(round(mass-0.1)-0.4,a,t0,ex,SampInt))
          xmax=round(m2t(round(mass-0.1)+0.6,a,t0,ex,SampInt))
          !P.MULTI = [0,1,2] 
          loadct,38
          if(drop1 lt 0.5) then  !x.oMARGIN=[20,20] else  !x.oMARGIN=[0,0]
          plot, m2t(x[xmin:xmax],a,t0,ex,SampInt),  data[xmin:xmax], /YNOZERO, xstyle=1,ystyle=1,  XTITLE='Da', YTITLE='signal' , color=0, background=-1,yrange=[0,max(data[xmin:xmax])+max(data[xmin:xmax])/10], charsize=1.5
          loadct,31
          oplot, [intstart,intstart],[min(data[xmin:xmax]),max(data[xmin:xmax])],color=28
          oplot, [intend,intend],[min(data[xmin:xmax]),max(data[xmin:xmax])],color=28
          oplot, m2t(x[xmin:xmax],a,t0,ex,SampInt),  baseline[xmin:xmax], color=28, thick=2 
          oplot, m2t(x[xmin:xmax],a,t0,ex,SampInt),  data2[xmin:xmax], color=35, thick=2 
          oplot, [pstart,pend],[max(data[xmin:xmax]),max(data[xmin:xmax])],color=35
          oplot, [pstart,pend],[min(data[xmin:xmax]),min(data[xmin:xmax])],color=35, thick=3
          oplot, [Pcenter,Pcenter],[max(data[xmin:xmax])-max(data[xmin:xmax])/20,max(data[xmin:xmax])+max(data[xmin:xmax])/20],color=35
          oplot, [Pcenter,Pcenter],[min(data[xmin:xmax]),min(data[xmin:xmax])+max(data[xmin:xmax])/20],color=35, thick=3
          xgauss= m2t(x[m2t(pstart,a,t0,ex,SampInt): m2t(pend,a,t0,ex,SampInt)],a,t0,ex,SampInt)
          ygauss=data2[m2t(pstart,a,t0,ex,SampInt): m2t(pend,a,t0,ex,SampInt)]
          weights=replicate(1.0, N_ELEMENTS(ygauss))
          AA=[max(ygauss),Pcenter,Pcenter/(4000* 2*SQRT(2*ALOG(2)))]
          xyouts, m2t(x[xmin],a,t0,ex,SampInt)+0.025,max(data[xmin:xmax]),t09str(EngData.starttime),color=0, charsize=1.1,charthick=2
          xyouts, m2t(x[xmin],a,t0,ex,SampInt)+0.025,max(data[xmin:xmax])*0.9,'m/z:'+string(mass,Format='(F8.3)'),color=0, charsize=1.1,charthick=2
          match=match(mass,lib)
          xyouts, m2t(x[xmin],a,t0,ex,SampInt)+0.180,max(data[xmin:xmax])*0.9,match.formula,color=28, charsize=1.1,charthick=2
          xyouts, m2t(x[xmin],a,t0,ex,SampInt)+0.025,max(data[xmin:xmax])*0.8,'Closest matches, dm [mDa]:',color=0, charsize=1.1,charthick=1
          for r=0,8 do if (abs(match.devi[r]) gt 3 or r ne 4) then begin
                  mis=match(mass+match.devi[r]/1000,lib)
                  xyouts, m2t(x[xmin],a,t0,ex,SampInt)+0.025,max(data[xmin:xmax])*(0.73-0.07*r),strjoin([string(match.devi[r],format='(F6.1)'),mis.formula],'  '),color=0, charsize=1.1
                  oplot, [Pcenter+match.devi[r]/1000,Pcenter+match.devi[r]/1000],[max(data[xmin:xmax])+max(data[xmin:xmax])/30,max(data[xmin:xmax])+max(data[xmin:xmax])/18],color=0
          endif else begin 
                  xyouts, m2t(x[xmin],a,t0,ex,SampInt)+0.025,max(data[xmin:xmax])*(0.73-0.07*r),strjoin([string(match.devi[r],format='(F6.1)'),'match'],'  '),color=28, charsize=1.1
                  oplot, [Pcenter+match.devi[r]/1000,Pcenter+match.devi[r]/1000],[max(data[xmin:xmax])+max(data[xmin:xmax])/30,max(data[xmin:xmax])+max(data[xmin:xmax])/12],color=28
                  oplot, [Pcenter+match.devi[r]/1000,Pcenter+match.devi[r]/1000],[max(data[xmin:xmax])+max(data[xmin:xmax])/12,max(data[xmin:xmax])+max(data[xmin:xmax])/10],color=28,thick=3
          endelse
     
          if(drop1 gt 0.5) then  begin
                  Name1=filepar.name1
                  S1=GetFileExport(event, destfolder, Name1)
                  if(max([max(S1.cpsFL),max(S1.corrcpsFL),max(S1.cpsUL),max(S1.corrcpsUL)]) eq -9999) then begin 
                          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), set_VALUE= 'file mass list' 
                          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), set_uVALUE= 'file mass list'                
                          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind1'), SET_VALUE=string(filepar.index, format='(I5)')
                          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind2'), SET_VALUE=string(filepar.index, format='(I5)')
                          export, event   
                          S1=GetFileExport(event, destfolder, Name1)
                  endif
                  tol=max([0.006,50*pcenter/1e6])
                  if(drop1 eq 1) then begin
                          if(max(S1.cpsFL) gt -9999) then begin
                                  filte=where(abs(S1.massFL-pcenter) lt tol)
                                  if(max(filte) gt -0.5) then datFL=reform(S1.cpsFL[filte,*]) else datFL=-9999
                          endif else datFL=-9999
                          if(max(S1.corrcpsFL) gt -9999 and max(datFL) ne -9999) then corrdatFL=reform(S1.corrcpsFL[filte,*]) else corrdatFL=-9999
                          if(max(S1.cpsUL) gt -9999) then begin
                                  filte=where(abs(S1.massUL-pcenter) lt tol)
                                  if(max(filte) gt -0.5) then datUL=reform(S1.cpsUL[filte,*]) else datUL=-9999
                          endif else datUL=-9999
                          if(max(S1.corrcpsUL) gt -9999 and max(datUL) ne -9999) then corrdatUL=reform(S1.corrcpsUL[filte,*]) else corrdatUL=-9999
                          if(max(datFL) ne -9999) then dat=datFL else if(max(datUL) ne -9999) then dat=datUL
                          ytit='cps'
                  endif             
                  if(drop1 eq 2) then begin
                          if(max(S1.ppbFL) gt -9999) then begin
                                  filte=where(abs(S1.massFL-pcenter) lt tol)
                                  if(max(filte) gt -0.5) then datFL=reform(S1.ppbFL[filte,*]) else datFL=-9999
                          endif else datFL=-9999
                          if(max(S1.corrppbFL) gt -9999 and max(datFL) ne -9999) then corrdatFL=reform(S1.corrppbFL[filte,*]) else corrdatFL=-9999
                          if(max(S1.ppbUL) gt -9999) then begin
                                  filte=where(abs(S1.massUL-pcenter) lt tol)
                                  if(max(filte) gt -0.5) then datUL=reform(S1.ppbUL[filte,*]) else datUL=-9999
                          endif else datUL=-9999
                          if(max(S1.corrppbUL) gt -9999 and max(datUL) ne -9999) then corrdatUL=reform(S1.corrppbUL[filte,*]) else corrdatUL=-9999
                          if(max(datFL) ne -9999) then dat=datFL else if(max(datUL) ne -9999) then dat=datUL
                          ytit='ppb'
                  endif             
                  
                 ; if (var_exists(dat) eq 1) then begin
                          if(max(S1.massUL) gt -9999) then begin
                                  Intlist=peaktable(S1.massUL,filepar.resolution)
                                  IntStart=Intlist[*,1]
                                  IntEnd=Intlist[*,2]
                                  xyouts,m2t(x[xmin],a,t0,ex,SampInt)+0.025,max(data[xmin:xmax])/15,'int. bound. UnifMassList:', color=28,  charthick=1
                                  xyouts,IntEnd,intarr(max(d(IntEnd)))+max(data[xmin:xmax])/15,strarr(max(d(IntEnd)))+'|', color=208,  charthick=3
                                  xyouts,IntStart,intarr(max(d(IntStart)))+max(data[xmin:xmax])/15,strarr(max(d(IntStart)))+'|', color=28,  charthick=1
                          endif
                          if(max(S1.massFL) gt -9999) then begin
                                  Intlist=peaktable(S1.massFL,filepar.resolution)
                                  IntStart=Intlist[*,1]
                                  IntEnd=Intlist[*,2]
                                  xyouts,m2t(x[xmin],a,t0,ex,SampInt)+0.025,0,'int. bound. FileMassList:', color=28,  charthick=1
                                  xyouts,IntEnd,intarr(max(d(IntEnd)))+0,strarr(max(d(IntEnd)))+'|', color=208,  charthick=3
                                  xyouts,IntStart,intarr(max(d(IntStart)))+0,strarr(max(d(IntStart)))+'|', color=28,  charthick=1
                          endif
                          
                   if (var_exists(dat) eq 1) then begin
                         
                          
                          loadct,38 
                         
                          plot, dat, /YNOZERO  ,  XTITLE='cycles',YTITLE=ytit , yrange=range(dat[where(dat ne -9999)]), color=0, background=-1, thick=2, charsize=1.5
                          if(max(corrdatUL) ne -9999) then oplot, corrdatUL, color=100,  thick=2
                          if(max(datUL) ne -9999) then oplot, datUL, color=50,  thick=2
                          if(max(corrdatFL) ne -9999) then oplot, corrdatFL, color=90,  thick=2
                          if(max(datFL) ne -9999) then oplot, datFL, color=40,  thick=2
                          if(max(datFL) ne -9999) then xyouts,0.91,0.44,'FL', color=40,/normal,  charthick=2
                          if(max(corrdatFL) ne -9999) then xyouts,0.91,0.415,'corr FL', color=90,/normal,  charthick=2
                          if(max(datUL) ne -9999) then xyouts,0.91,0.39,'UL', color=50,/normal,  charthick=2
                          if(max(corrdatUL) ne -9999) then xyouts,0.91,0.365,'corr UL', color=100,/normal,  charthick=2
                          makecsv,DestDir+'temp\last_cps_row.csv' ,dat
                          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= ['Data (no overlap correction):', reform(string(dat[0,*]))]
  
                        
                 endif else plot, [0,0],[0,0]
          endif else begin ; print derivdata
                  loadct,38
                  plot, m2t(x[xmin:xmax],a,t0,ex,SampInt),  data[xmin:xmax], /YLOG, /YNOZERO, xstyle=1,ystyle=1,  XTITLE='Da', YTITLE='signal' , color=0, background=-1,yrange=[min(data[xmin:xmax])*0.9,max(data[xmin:xmax])+max(data[xmin:xmax])/10], charsize=1.5
                  loadct,31
                  oplot, m2t(x[xmin:xmax],a,t0,ex,SampInt),  baseline[xmin:xmax], color=28, thick=2
                  oplot, m2t(x[xmin:xmax],a,t0,ex,SampInt),  data2[xmin:xmax], color=35, thick=2
                  oplot, [Pcenter,Pcenter], [min(data[xmin:xmax]),max(data[xmin:xmax])], color=28
            ;      oplot,m2t([x[xmin],x[xmax]],a,t0,ex,SampInt),[0,0],color=0
            ;      oplot, [Pcenter,Pcenter],[-max(derivdata[xmin:xmax])/10, max(derivdata[xmin:xmax])/10],color=28, thick=3
            ;      oplot, [pstart,pend],[0,0],color=28, thick=3
          endelse
  endif

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;   END
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
  


PRO plot3, event
;called by button 'Plot Mass Peaks'
; plots unified masslist: x= integer mass, y= fractional deviation from integer mass
DEVICE, DECOMPOSED = 0 
loadct,38
!P.MULTI = [0,1,1] 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestFolder 
 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), gET_uVALUE= masses
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_2'), gET_uVALUE= UniList
  if (var_exists(masses) eq 1 ) then begin
 if(min(masses) eq 1 or min(masses) eq 1.8) then   if (max(unilist.peakloc) ne -9999 ) then begin
          plot,UniList.PpmBINscale,UniList.PeakCountsPERppmBINS, background=-1,color=0, XTITLE='bin mass scale [Da] (bin size: max of 8 ppm or 1mDa)', YTITLE='# of Files with peak in bin', charsize=2,thick=2
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string(indgen(1300))
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= indgen(1300)
endif else unimasslist,event

 if(min(masses) eq 0) then  begin
     masses=readcsv(DestFolder+'UnifiedMassList.csv')
  xpeaks=masses[*,0]
 
    ; xpeaks=UniList.PeakLoc 
     WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string([1,xpeaks], format='(F8.3)')
     WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= [1,xpeaks]
  endif 
  
   if(min(masses) gt 2) then  begin
     WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_5'), gET_uVALUE= filepar
     xpeaks=filepar.masslist[*,0]
     WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string([1.8,xpeaks], format='(F8.3)')
     WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= [1.8,xpeaks]
  endif

 if(min(masses) ne 1 and min(masses) ne 1.8) then  begin
      x=floor(xpeaks+0.4)
      y=1000*(xpeaks-x)
      filter=where(2*floor(x/2) eq x) ;selects even masses
      plot, x,y,psym = 4, thick=5, xrange=[0,360],yrange=[-100,400], color=0, background=-1, XTITLE='integer mass [Da]', YTITLE='deviation from integer mass [mDa]', charsize=2
      loadct,31
      for i=1,350 do oplot, [i,i]*2, [-500,800], color=203
      for i=-9,35 do oplot, [0,800],[i,i]*50, color=84
      for i=1,150 do oplot, [i,i]*10, [-500,800], color=84
      oplot, x[filter],y[filter],color=28, psym = 4, thick=4
      filter=where(2*floor(x/2) ne x) ;selects odd masses
      oplot, x[filter],y[filter],color=16, psym = 4, thick=4
      oplot, [0,800],1.1161*[0,800] + 6.6339, color=16, thick=2
      oplot, [0,800],1.1122*[0,800] - 43.173, color=16, thick=2
endif
endif else print, 'No Mass List'

end


 
 PRO PTRwid_event, event
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_CONTEXT') THEN BEGIN
    contextBase = WIDGET_INFO(event.ID, FIND_BY_UNAME = 'base_3')
    WIDGET_DISPLAYCONTEXTMENU, event.ID, event.X, event.Y, contextBase
  ENDIF
END
 




pro selList,event
;called by selecting item from droplist
 
 WIDGET_CONTROL, event.id, get_VALUE= MassListOption
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), set_VALUE= MassListOption 
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_8'), set_uVALUE= MassListOption 
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestFolder 
  
  if (MassListOption eq 'file mass list') then begin
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_5'), gET_uVALUE= Filepar
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), sET_VALUE= string(Filepar.masslist[*,0], format='(F10.3)')
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), sET_uVALUE= Filepar.masslist[*,0]
 endif
 
 if (MassListOption eq 'unified mass list') then begin
    masses=readcsv(DestFolder+'UnifiedMassList.csv')
  masses=masses[*,0]
  
       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string(masses , format='(F10.3)')
       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= masses 
 endif
 
 if (MassListOption eq 'unit mass list') then begin
       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_5'), gET_uVALUE= Filepar
       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string(findgen(Filepar.maxmass-4)+3)
       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= findgen(Filepar.maxmass-4)+3
 endif
  
 if (MassListOption eq 'user mass list') then begin
       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), gET_VALUE= Destdir
       Path=dialog_pickfile( TITLE='Select mass list file' ,path=Destdir)
       if (path NE '') then begin
            m_list=readCsv(Path)
            WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), sET_VALUE= string(m_list[*,0])
            WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), sET_uVALUE= m_list[*,0]
       endif
 endif
   
  
  
   
end


pro setdrop1,event
;called by selecting item from droplist
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Drop_1'), set_uVALUE= event.index  
   print, 'jjssj'
  
  if (event.index eq 0) then  print,'no timeline' 
   if (event.index eq 1) then  print,'cps timeline chosen'
   if (event.index eq 2) then  print,'ppb timeline chosen'
          
  
end


PRO SingleFile, event
; called by clicking a file from the file list.
; performs single file evaluation 
  WIDGET_CONTROL, /HOURGLASS 
  runtime_strt=systime(1)
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestFolder 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), GET_uVALUE=Files
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'label_2'), gET_uVALUE= Names
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), gET_VALUE= log
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), gET_VALUE= info
         
  i=event.index
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind1'), SET_VALUE=string(i, format='(I5)')
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Ind2'), SET_VALUE=string(i, format='(I5)')
 
 ;delete data from previous file
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_6'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_7'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_1'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_2'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_3'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_4'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_5'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_6'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_10'), set_uVALUE= -9999 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_12'), set_uVALUE= -9999 
                   
 
 
  
  Name1=Names[i]
  EngData=getengdata(event, files[i])
 ;EngData=getsumspec(files[i])
 Print, 'engdat'
  print, systime(1)-runtime_strt
  runtime_OpenFile=systime(1)-runtime_strt
  if(engdata.duration ne -9999) then begin; reading engdata was successful....
          NamePar=''
          exists=file_info(destfolder+'FileInfo/Time2MassPAR/'+name1+'PAR2.fdt') & exists=exists.exists
          exists3=file_info(destfolder+'FileInfo/IonList/'+name1+'FL2.fdt') & exists3=exists3.exists
          if (exists eq 1)then NamePar=destfolder+'FileInfo/Time2MassPAR/'+name1+'PAR2.fdt' 
          if (exists3 eq 1)then NameMsss=destfolder+'FileInfo/IonList/'+name1+'FL2.fdt' 
          if (NamePar ne '') then   if (max(readfloat(NamePar)) eq -9999) then NamePar=''
          if (NamePar ne '') then begin       
                  par=readfloat(NamePar)
                  a=par[0]
                  t0=par[1]
                  ex=par[2]
                  maxmass=par[3]
                  msss=readfloat(NameMsss)
                  baseline=readfloat(destfolder+'FileInfo/Baseline/'+name1+'Baseline.fdt') 
                  ResolVsMass = readfloat(destfolder+'FileInfo/PeakShape/'+name1+'ResolutionVsMass.fdt')  ;---MB
                  Filepar=create_struct('file',files[i],'name1',name1,'index',i,'baseline',baseline,'maxmass', maxmass,'a',a,'t0',t0,'ex',ex,'masslist',msss,'masslistnames',['PeakMax[Da]','StartPeak[Da]','EndPeak[Da]','PeakBroadness','SlopeMax','SlopeMin','PeakHight[counts]','PeakMax[time]','BaseHight[counts]'],'resolution',par[8],'ResolutionVsMass',ResolVsMass); ---MB   
  
 
 Print, 'filepar'
   print, systime(1)-runtime_strt
 
          endif else begin
                  S1=PS1(event,EngData.SumSpec,EngData.SampInt,EngData.duration,EngData.cycles, EngData.extractions, destfolder,Name1,  EngData.instrument)
 ;masslistnames=['PeakMax[Da]','StartPeak[Da]','EndPeak[Da]','Resolution','SlopeMax','SlopeMin','PeakHight[counts]','PeakMax[time]','BaseHight[counts]']
;    s1=CREATE_STRUCT('MaxMass',maxMass,'a', a,'t0',t0,'ex',ex,'scorParVar',scorParVar,'a3', a3,'t03',t03,'ex3',ex3,'scor3',scor3,'massnames',massnames,$
;    'counts', counts[1:10],'res',res[1:10], 'ErrCode', ErrCode,'masslistnames',masslistnames,'MassList',MassList, $
;    'baseline', baselineSM/extr, 'PeakShape',[[x7],[dataX3]],'resolution',reso)
                  Filepar=create_struct('file',files[i],'name1',name1,'index',i,S1)
                  if(max(Filepar.counts) gt 0) then Filepar.counts[where(Filepar.counts gt 0)]=Filepar.counts[where(Filepar.counts gt 0)]/EngData.duration
          endelse   
          if(Filepar.a ne -9999) then begin  ;don't do stuff below if calcrude failed
                  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_EngDat'), SET_VALUE= EngData.names
                  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_EngDat'), SET_uVALUE= EngData
                  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string(Filepar.masslist[*,0],FormaT='(f9.3)')
                  info=[info,'',string(Filepar.masslist[*,0],FormaT='(f12.3)')]
                  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), SET_VALUE= info
         
                
                  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= Filepar.masslist[*,0]
                  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_5'), SET_uVALUE= Filepar
                  DetectedPeaks=size(Filepar.masslist,/dimensions)
                  OutputString=[strjoin(['#',string(i, format='(I5)'),':  ', Name1]),$
                  strjoin(['Cycles:        ',string(engdata.cycles)]) ,$
                  strjoin(['Duration [min]:   ',string(engdata.duration/60, format='(F8.2)')]) ,$
                  strjoin(['MaxSignal:   ',string( max(EngData.SumSpec), format='(E10.2)')]) ,$
                  strjoin(['MaxSignal/s:   ',string( max(EngData.SumSpec)/engdata.duration, format='(I10)')]) ,$
                  strjoin(['StrtTime [T09]:   ',string(engdata.starttime, format='(F9.3)')]) ,$
                  strjoin(['Maxmass [Da]:',string(Filepar.maxmass)]) ,$
                  strjoin(['a:                     ',string(Filepar.a)]) , $
                  strjoin(['t0:                    ',string(Filepar.t0)]) ,$
                  strjoin(['ex:                    ',string(Filepar.ex)]) , $
                  strjoin(['Resolution (FWHM):     ',string(Filepar.resolution, Format='(I5)')]) , $
                  strjoin(['Detected peaks:', string(max(DetectedPeaks))]),$
                  strjoin(['Total processing time: ',string(systime(1)-runtime_strt , format='(F5.1)'),'s']),'------------','']
                  if (NamePar ne '') then begin       
                          log=[OutputString, log] 
                  endif else begin
                          testdat=strjoin([Filepar.massnames[0],string(Filepar.counts[0], format='(I10)'),string(Filepar.res[0], format='(I10)')])
                          for ij=1,9 do testdat=[testdat,strjoin([Filepar.massnames[ij],string(Filepar.counts[ij], format='(I10)'),string(Filepar.res[ij], format='(I10)')])]
                          log=[OutputString,'Test: mass, [cps], [FWHM]',testdat,'------------','', log ]   
                          makefloat, destfolder+'FileInfo\IonList\'+name1+'FL2.fdt', Filepar.masslist 
                          makefloat, destfolder+'FileInfo\Time2MassPAR\'+name1+'PAR2.fdt', [Filepar.a,Filepar.t0,Filepar.ex,filepar.maxmass, EngData.SampInt,Filepar.a3,Filepar.t03,Filepar.ex3,Filepar.resolution,strpos('H3O+ NO+ O2+',filepar.mode)]
                          makefloat, destfolder+'FileInfo\PeakShape\'+name1+'PeakShape.fdt', Filepar.PeakShape
                          makefloat, destfolder+'FileInfo\BaseLine\'+name1+'Baseline.fdt', Filepar.baseline
                          makefloat, destfolder+'FileInfo\PeakShape\'+name1+'ResolutionVsMass.fdt', Filepar.ResolutionVsMass ;---MB

                  endelse
          endif else begin ;calcrude failed
                  OutputString=[strjoin(['#',string(i, format='(I5)'),':  ', Name1]),$
                    'Crude calibration FAILED', '------------','']
                  log=[OutputString, log] 
          endelse        
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE= log
          Print, 'fertig'
   print, systime(1)-runtime_strt
 
  endif ; read engdata failed
END



pro UniMassList, event
; called by button 'Unified Mass List'
;sumspectrum of each file is evaluated: Peak search, mass scale calibration, parameters a, t0, and ex are optimized for each file 
;after single file evaluation a unified mass list is created and saved
; TO RE-RUN THE SINGLE FILE EVALUATION: remove the 'PeakCountsPermDa' file from the destination folder 
compile_opt idl2
 WIDGET_CONTROL, /HOURGLASS 
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), GET_uVALUE=lib 
 if (max(lib) eq -9999) then  begin 
          lib=masslib(/extended)
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), sET_uVALUE=lib
  endif
  
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DestDir'), GET_VALUE=DestFolder 
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Files'), GET_uVALUE=Files
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'label_2'), gET_uVALUE= Names
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), gET_VALUE= log
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= ''                         
 NumFiles=max(size(Files,/dimensions))
 time_process=0
 TimePerFile=15
 NewFile=0
 Lastcheck=max(readfloat(strjoin([DestFolder,'Lastcheck.fdt'],'')))
 Now=max(t09('01/01/1970, 00:00:00')+systime(/seconds)/(24.0*3600.0))
 exsts1=file_info(strjoin([DestFolder,'binScale.fdt'],'')) & exsts1=exsts1.exists
 exsts2=file_info(strjoin([DestFolder,'resolution.fdt'],'')) & exsts2=exsts2.exists
 exsts3=file_info(strjoin([DestFolder,'PeakPar.fdt'],'')) & exsts3=exsts3.exists
 
 if(now-lastcheck gt 0.5 or exsts1+exsts2+exsts3 ne 3) then begin
        makefloat, DestFolder+'Lastcheck.fdt',fltarr(1)+Now
        if(exsts1+exsts2+exsts3 ne 3) then NewFile=1
        for i=0,NumFiles-1 do begin
                exists=file_info(destfolder+'FileInfo/IonList/'+names[i]+'FL2.fdt') & exists=exists.exists
                if (exists eq 0) then begin ; no par file
                        NewFile=1
                        runtime_strt=systime(1)
                        Sumspec=getsumspec(files[i])
                        if(sumspec.duration  ne -9999 and sumspec.duration  gt 0) then begin                    
                                jj=0
                                duration=sumspec.duration
                                sumspectr=SumSpec.sumspec
                                nextStartTime=SumSpec.starttime+duration/(3600*24)
                                sig=max(smooth(sumspectr,3))
                                while(sig lt getpar('Desired_Min_Signal')) do begin
                                        IF (i+jj+1 gt NumFiles-1) THEN BREAK 
                                        next=getsumspec(files[i+jj+1 ])
                                        if(next.duration  eq -9999) then BREAK     
                                        if(abs(next.starttime-nextStartTime)*24*60  gt getpar('Max_Time_Gap')) then BREAK    
                                        jj=jj+1
                                        duration=duration+next.duration
                                        sumspectr=sumspectr+next.sumspec
                                        sig=sig+max(smooth(next.sumspec,3))
                                        nextStartTime=next.starttime+next.duration/(3600*24)
                                endwhile 
                                runtime_OpenFile=systime(1)-runtime_strt        
                                if(duration gt 0) then begin
                                        goodF=0
                                        fls= names[i]
                                        if(jj gt 0.5) then for kk=1,jj do fls=[fls, Names[i+kk ]]
                                        Filepar=PS1(event,sumspectr, sumspec.SampInt, sumspec.duration, sumspec.cycles, sumspec.extractions,destfolder,fls,sumspec.instrument)
                                        if(max(Filepar.counts) ne -9999)then  Filepar.counts[where(Filepar.counts gt 0)]=Filepar.counts[where(Filepar.counts gt 0)]/duration
                                        if (max(Filepar.res)gt 1800) then goodF=1
                                        if (max(Filepar.a) ne -9999) then begin
                                                time_process=time_process+systime(1)-runtime_strt
                                                TimePerFile=[TimePerFile,systime(1)-runtime_strt]
                                                DetectedPeaks=size(Filepar.masslist,/dimensions)
                                                testdat=strjoin([Filepar.massnames[0],string(Filepar.counts[0], format='(I10)'),string(Filepar.res[0], format='(I10)')])
                                                for ij=1,9 do testdat=[testdat,strjoin([Filepar.massnames[ij],string(Filepar.counts[ij], format='(I10)'),string(Filepar.res[ij], format='(I10)')])]
                                                log=[fls,$
                                                  strjoin(['StrtTime [T09]:   ',string(sumspec.starttime, format='(F9.3)')]) ,$
                                                  strjoin(['Duration [min]:   ',string(duration/60, format='(F8.2)')]) ,$
                                                  strjoin(['MaxSignal:   ',string( max(sumspectr), format='(E10.2)')]) ,$
                                                  strjoin(['MaxSignal/s:   ',string( max(sumspectr)/duration, format='(I10)')]) ,$
                                                  strjoin(['Maxmass [Da]:',string(Filepar.maxmass)]) ,$
                                                  strjoin(['a:                     ',string(Filepar.a)]) , $
                                                  strjoin(['t0:                    ',string(Filepar.t0)]) ,$
                                                  strjoin(['ex:                    ',string(Filepar.ex)]) , $
                                                  strjoin(['Resolution (FWHM):     ',string(Filepar.resolution, Format='(I5)')]) , $
                                                  strjoin(['Detected peaks:', string(max(DetectedPeaks))]),$
                                                  '------------',$
                                                  '',$
                                                  'Test: mass, [cps], [FWHM]',$
                                                  testdat,$
                                                  strjoin(['Total processing time: ',string(systime(1)-runtime_strt , format='(F5.1)'),'s']),$   
                                                  strjoin(['Files remaining: ',string((NumFiles-i-1) , format='(I4)')]),$   
                                                  strjoin(['Time remaining: ',string(quantile(TimePerFile,0.8)*(NumFiles-i-1) , format='(I6)'),'s']),'------------','', log ]   
                                                  if(max(size(log,/dimensions)) gt 100) then log=log[0:99]
    
                                        endif else begin
                                                log=[fls,'Crude calibration FAILED', '------------','', log] 
                                                if(max(size(log,/dimensions)) gt 100) then log=log[0:99]
    
                                        endelse
                                        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE= log
                                        for kk=0,jj do if(goodF eq 1) then  makefloat, destfolder+'FileInfo/IonList/'+names[i+kk]+'FL2.fdt', Filepar.masslist else  makefloat, destfolder+'FileInfo/IonList/'+names[i+kk]+'FL2.fdt', [-9999]
                                        for kk=0,jj do if(goodF eq 1) then makefloat, destfolder+'FileInfo/Time2MassPAR/'+names[i+kk]+'PAR2.fdt', [Filepar.a,Filepar.t0,Filepar.ex,Filepar.maxmass,sumspec.SampInt,Filepar.a3,Filepar.t03,Filepar.ex3,Filepar.resolution, strpos('H3O+ NO+ O2+',Filepar.mode)] else makefloat, destfolder+'FileInfo/Time2MassPAR/'+names[i+kk]+'PAR2.fdt', [-9999] 
                                        for kk=0,jj do if(goodF eq 1) then makefloat, destfolder+'FileInfo/PeakShape/'+names[i+kk]+'PeakShape.fdt', Filepar.PeakShape else makefloat, destfolder+'FileInfo/PeakShape/'+names[i+kk]+'PeakShape.fdt', [-9999] 
                                        for kk=0,jj do if(goodF eq 1) then makefloat, destfolder+'FileInfo/Baseline/'+names[i+kk]+'Baseline.fdt', Filepar.baseline else makefloat, destfolder+'FileInfo/Baseline/'+names[i+kk]+'Baseline.fdt', [-9999] 
                                        i=i+jj
                                endif
                        endif
                endif 
        endfor ; loop over files
        if(max(getpar('HardCal') ) eq 1) then begin
                callist=[getpar('M1'),getpar('M2'),getpar('M3'),getpar('M4'),getpar('M5'),getpar('M6'),getpar('M7'),getpar('M8'),getpar('M9'),getpar('M10'), $
                  getpar('M11'),getpar('M12'),getpar('M13'),getpar('M14'),getpar('M15'),getpar('M16'),getpar('M17'),getpar('M18'),getpar('M19'),getpar('M20'),$
                  getpar('M21'),getpar('M22'),getpar('M23'),getpar('M24'),getpar('M25'),getpar('M26'),getpar('M27'),getpar('M28'),getpar('M29'),getpar('M30')]
                callist=callist[where(callist gt 10)]
                print,callist
                print, 'hier'
                for i=0,NumFiles-1 do begin
                        exists=file_info(destfolder+'FileInfo/IonList/'+names[i]+'FL2.fdt') & exists=exists.exists
                        if (exists ne 0) then begin 
                                FL2=readfloat(destfolder+'FileInfo/IonList/'+names[i]+'FL2.fdt')
                                PAR2=readfloat(destfolder+'FileInfo/Time2MassPAR/'+names[i]+'PAR2.fdt')
                                if( max(size(par2,/dimensions)) gt 4.5) then sampint=par2[4] else sampint=1e-10            
                                if(FL2[0] gt 0) then masslist=FL2[*,0]
                                timelistold=m2t(masslist,par2[0],par2[1], par2[2],sampint)
                                timelist=m2t(callist,par2[0],par2[1], par2[2],sampint)
                                S1=testSCALE(timelist,masslist, 500,par2[0],par2[1], par2[2],sampint)
                                pr1=S1.scorppm
                                ppm=1e6*S1.deviation/S1.masses
                                scr=mean(abs(ppm[where(abs(ppm) lt 60)]))
                                filter=where(abs(ppm) lt 200)
                                !P.MULTI = [0,1,2] 
                                filter=where(abs(ppm) lt 200)
                                plot, S1.masses[filter],ppm[filter],yrange=[-200,200], psym=4
                                oplot, [0,1000],[0,0] 
                                xyouts, 50, 150, 'file '+string(i,format='(I4)')+' (of '+string(NumFiles,format='(I4)')+'), time:'+string(systime(1)-mistt,format='(I4)')
                                ; print, [i,S1.scorppm, S1.scor]
                                a_raw=par2[0]
                                t0_raw=par2[1]
                                a=par2[0]
                                t0=par2[1]
                                ex=par2[2]
                                for r=0,1 do begin
                                        a_raw=a
                                        t0_raw=t0
                                        ex_raw=ex
                                        for q=-3,3 do begin
                                                mag=[1,3]
                                                ex1=ex_raw+0.00001*q/mag[r]
                                                grid=29
                                                stepa=2.002/(grid*mag[r])
                                                stept0=20.01/(grid*mag[r])
                                                if(grid eq 2*floor(grid/2)) then grid=grid+1
                                                sco=findgen(grid,grid)/1e8
                                                for k=-floor(grid/2),floor(grid/2) do begin
                                                        aa=a_raw+stepa*k
                                                        for l=-floor(grid/2),floor(grid/2) do begin
                                                                t00=t0_raw+stept0*l
                                                                tst=testSCALE2(timelist,masslist, 450,aa,t00,ex1, SampInt)
                                                                sco[k+floor(grid/2),l+floor(grid/2)]=tst.scor
                                                        endfor
                                                endfor    
                                                maxi=where(sco eq max(sco))
                                                l=floor(maxi/grid)
                                                k=maxi-grid*l
                                                sco[floor(grid/2),floor(grid/2)]=0
                                                a1=max(a_raw+stepa*(k-floor(grid/2)))
                                                t01=max(t0_raw+stept0*(l-floor(grid/2)))
                                                old=testSCALE2(timelist,masslist, 450,a,t0,ex, SampInt)
                                                new=testSCALE2(timelist,masslist, 450,a1,t01,ex1, SampInt)
                                                ;     print,q, a, t0, ex, old.scor
                                                ; print, q, a1, t01, ex1, new.scor
                                                if(new.scor gt old.scor) then begin
                                                        a=a1
                                                        t0=t01
                                                        ex=ex1
                                                endif
                                        endfor
                                endfor
                                S1=testSCALE(timelist,masslist, 500,a,t0,ex,sampint)
                                print,[i,pr1, S1.scorppm, systime(1)-mistt]
                                ppm=1e6*S1.deviation/S1.masses
                                scr=mean(abs(ppm[where(abs(ppm) lt 60)]))
                                filter=where(abs(ppm) lt 200)
                                plot, S1.masses[filter],ppm[filter],yrange=[-200,200], psym=4
                                oplot, [0,1000],[0,0] 
                                xyouts, 50, 150, 'file '+string(i,format='(I4)')+' (of '+string(NumFiles,format='(I4)')+'), time:'+string(systime(1)-mistt,format='(I4)')
                                makefloat, destfolder+'FileInfo/IonList/'+names[i]+'FL3.fdt',  m2t(timelistold,a,t0,ex,sampint)
                                makefloat, destfolder+'FileInfo/Time2MassPAR/'+names[i]+'PAR3.fdt',[a,t0,ex,m2t(max(timelistold),a,t0,ex,sampint), sampint]
                                filter=where(ppm gt 40)
                        endif
                endfor
        endif   ;end Hardcal
 endif       
 runtime_strt= systime(1)
 if(NewFile eq 1) then begin
        masslist=float(0)
        resolution=float(0)
        for i=0,NumFiles-1 do begin
                exists=file_info(destfolder+'FileInfo/IonList/'+names[i]+'FL2.fdt') & exists=exists.exists
                if (exists ne 0) then begin 
                        FL2=readfloat(destfolder+'FileInfo/IonList/'+names[i]+'FL2.fdt')
                        PAR2=readfloat(destfolder+'FileInfo/Time2MassPAR/'+names[i]+'PAR2.fdt')
                        if(PAR2[0] gt 0) then resolution=[resolution,PAR2[8]]
                        if(FL2[0] gt 0) then masslist=[masslist,FL2[*,0]]
                endif
        endfor
        resolution=resolution[1:*]
        binScale=ppm_bin(masslist,8)
        x=binscale[*,0]
        y=binscale[*,2]
        peakdata=ps2(x,y)
        masslist=peakdata.data[*,1]     
        Intlist=PeakTable(masslist,median(resolution))
        peakdat=peakdata.data
        filter=where(intlist[*,2] gt 0.007+intlist[*,1])
        masslist=masslist[filter,*]
        intlist=intlist[filter,*]
        peakdat=peakdat[filter,*]
        makefloat, strjoin([DestFolder,'binScale.fdt'],''),binScale
        makefloat, strjoin([DestFolder,'resolution.fdt'],''),resolution
        makecsv, strjoin([DestFolder,'UnifiedMasslist.csv'],''), transpose(Intlist)
        makecsv, strjoin([DestFolder,'UnifiedMasslistNames.csv'],''), transpose(['mass[Da]','StartInt','EndInt','sigma','StartCut [fraction of sigma]','EndCut [fraction of sigma]'])
        makefloat, strjoin([DestFolder,'PeakPar.fdt'],''),peakdat
        makeCsv, DestFolder+'PeakPar.csv',TRANSPOSE(peakdat )
        makecsv, strjoin([DestFolder,'PeakParNames.csv'],''), transpose(peakdata.names)
 endif else begin 
        binscale=readfloat(strjoin([DestFolder,'binScale.fdt'],''))
        resolution=readfloat(strjoin([DestFolder,'resolution.fdt'],''))
        x=binscale[*,0]
        y=binscale[*,2]
        peakdat=readfloat(strjoin([DestFolder,'PeakPar.fdt'],''))
 endelse
 xpeaksDer=peakdat[*,0]
 xpeaks=peakdat[*,1]
 sigma=peakdat[*,2]
 ymax=peakdat[*,3]
 Unified_Mass_List=CREATE_STRUCT('PeakCountsPERppmBINS',y,'PpmBINscale',x,'PeakLoc',xpeaks,'PeakLocDer',xpeaksDer,'PeakWidth',sigma,'PeakHeight',ymax)
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_2'), SET_uVALUE= Unified_Mass_List
 DEVICE, DECOMPOSED = 0 
 loadct,38
 !P.MULTI = [0,1,1] 
 Plotsel=max(readfloat(strjoin([DestFolder,'PlotSel.fdt'],'')))
 if(Plotsel eq 1) then begin
        plot,x,y, background=-1,color=0, XTITLE='bin mass scale [Da] (bin size: max of 8 ppm or 1mDa)', YTITLE='# of Files with peak in bin', charsize=2,thick=2
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string(indgen(1300))
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= indgen(1300)
        makefloat,DestFolder+'PlotSel.fdt',fltarr(5)
 endif else begin
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_VALUE= string([1,xpeaks], format='(F8.3)')
        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'List_Masses'), SET_uVALUE= [1,xpeaks]
        x=floor(xpeaks+0.4)
        y=1000*(xpeaks-x)
        filter=where(2*floor(x/2) eq x) ;selects even masses
       ; plot, x,y,psym = 4, thick=5, xrange=[0,360],yrange=[-100,400], color=0, background=-1, XTITLE='integer mass [Da]', YTITLE='deviation from integer mass [mDa]', charsize=2
        plot, x,y,psym = 4, thick=5,  color=0, background=-1, XTITLE='integer mass [Da]', YTITLE='deviation from integer mass [mDa]', charsize=2
        loadct,31
        for i=1,350 do oplot, [i,i]*2, [-500,800], color=203
        for i=-9,35 do oplot, [0,800],[i,i]*50, color=84
        for i=1,150 do oplot, [i,i]*10, [-500,800], color=84
        oplot, x[filter],y[filter],color=28, psym = 4, thick=4
        filter=where(2*floor(x/2) ne x) ;selects odd masses
        oplot, x[filter],y[filter],color=16, psym = 4, thick=4
        oplot, [0,800],1.1161*[0,800] + 6.6339, color=16, thick=2
        oplot, [0,800],1.1122*[0,800] - 43.173, color=16, thick=2
        makefloat,DestFolder+'PlotSel.fdt',fltarr(5)+1
 endelse
 
 if(NewFile eq 1) then begin
        binscale=readfloat(strjoin([DestFolder,'binScale.fdt'],''))
        ; resolution=readfloat(strjoin([DestFolder,'resolution.fdt'],''))
        x=binscale[*,0]
        y=binscale[*,2]
        len=size(intlist,/dimensions) & len=max(len[0])
        IDnamesA=['m/z','sigma [ppm ]', 'sigma [mDa]','#compounds within +/- 2 sigma','dev [mDa]','#C','#13C','#H','#O','#N','#H+','dev [mDa]','#C','#13C','#H','#O','#N','#H+','dev [mDa]','#C','#13C','#H','#O','#N','#H+','dev [mDa]','#C','#13C','#H','#O','#N','#H+','dev [mDa]','#C','#13C','#H','#O','#N','#H+','dev [mDa]','#C','#13C','#H','#O','#N','#H+','dev [mDa]','#C','#13C','#H','#O','#N','#H+','dev [mDa]','#C','#13C','#H','#O','#N','#H+','dev [mDa]','#C','#13C','#H','#O','#N','#H+']
        IDA=strarr(len,max(size(idnamesA,/dimensions)))
        IDnamesB=['m/z','sigma [ppm]', 'sigma [mDa]','#compounds within +/- 2 sigma','dev1 [mDa]','ID1','dev2 [mDa]','ID2','dev3 [mDa]','ID3','dev4 [mDa]','ID4','dev5 [mDa]','ID5','dev6 [mDa]','ID6','dev7 [mDa]','ID7','dev8 [mDa]','ID8','dev9 [mDa]','ID9']
        IDB=strarr(len,max(size(idnamesB,/dimensions)))
        for i=0,len-1 do begin
                mass=masslist[i]
                sigm=sigma[i]
                filter=where(x gt mass-0.15 and x lt mass+0.15)
                ysm=smooth(y,5)
                x1=x[filter]
                y1=y[filter]
                filter=where(abs(x-mass) eq min(abs(x-mass)))
                filter=lindgen(11)-5+max(filter)
                x2=x[filter]
                y2=y[filter]
                y2sm=ysm[filter]
                if(max(y2)le 10 ) then  begin
                        r=gaussfit(x2,y2, nterms=3,coeff) 
                        str='P3'
                endif else begin 
                        r=gaussfit(x2,y2+0.0001, coeff)
                        str='P6'
                endelse
                if(coeff[0] lt 0 or coeff[0] gt max(y2)*3 or  1e6*abs(coeff[2])/mean(x2) lt 5  or  max(y2)le 5 ) then begin
                        r=gaussfit(x2,y2sm, nterms=3,coeff)
                        str='P3, SM5'
                endif    
                matchi=match(mass,lib)
                IDA[i,0]=string(mass,format='(F8.3)')
                IDA[i,1]=string(1e6*coeff[2]/mass,format='(F8.1)')
                IDA[i,2]=string(1000*coeff[2],format='(F8.1)')
                IDB[i,0]=string(mass,format='(F8.3)')
                IDB[i,1]=string(1e6*coeff[2]/mass,format='(F8.1)')
                IDB[i,2]=string(1000*coeff[2],format='(F8.1)')
                rr=0
                for r=0,8 do begin
                        mis=match(mass+matchi.devi[r]/1000,lib)
                        if (abs(matchi.devi[r])/1000 lt 2*coeff[2]) then begin 
                                IDB[i,4+2*rr]= string(matchi.devi[r],format='(F8.1)')      
                                IDB[i,5+2*rr]= mis.formula        
                                IDA[i,4+7*rr]= string(matchi.devi[r],format='(F8.1)')      
                                IDA[i,5+7*rr]= string(mis.entry[1],format='(I4)')        
                                IDA[i,6+7*rr]= string(mis.entry[2],format='(I4)')        
                                IDA[i,7+7*rr]= string(mis.entry[3],format='(I4)')        
                                IDA[i,8+7*rr]= string(mis.entry[4]  +mis.entry[5]+mis.entry[6],format='(I4)')      
                                IDA[i,9+7*rr]= string(mis.entry[7]  +mis.entry[8],format='(I4)')     
                                IDA[i,10+7*rr]=string(mis.entry[9],format='(I4)')      
                                rr=rr+1
                        endif
                endfor
                IDA[i,3]=string(rr,format='(I8)')
                IDB[i,3]=string(rr,format='(I8)')
        endfor
        makecsv, strjoin([DestFolder,'PeakIdentification_formulas.csv'],''), transpose([transpose(IDnamesB),[IDB]])
        makecsv, strjoin([DestFolder,'PeakIdentification_numbers.csv'],''), transpose([transpose(IDnamesA),[IDA]])
 endif
 print,'fertig'
end


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;                   PROCPTR routines
;;;;;;;
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO ____________PTRproc
 end


PRO ProcPTR, event


  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Buti1'), GET_uVALUE=path 
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Buti2'), GET_uVALUE=lib 
  Files=Filelst(path)
  
  masses=readcsv(path+'UnifiedMassList.csv')
  masses=masses[*,0]
  engnames=readcsvstr(path+'Export\EngData\'+'Engdatanames.csv')
 if(strpos(path,'w_data') lt -0.5) then path='click the right mouse button to select "w_data" directory'
  ;base = WIDGET_BASE(/Row, XSIZE = 1050, YSIZE = 750)
 
      
    base = WIDGET_BASE(/Row, XSIZE = 210, YSIZE = 700, RESOURCE_NAME = 'Merge data')
        base2=widget_base(base,/column,xsize=200,ysize=700)
              label1=Widget_label(base2,/Align_left, value="Directory:",uname='Directory',uvalue=-9999)
              wText = WIDGET_TEXT(base2, VALUE=path, uname="textfield", /EDITABLE,/ALL_EVENTS, /NO_NEWLINE,/wrap , /CONTEXT_EVENTS, YSIZE = 2, uVALUE= lib)
                      contextBase1 = WIDGET_BASE(wText,  /CONTEXT_MENU,  UNAME="contextMenu")
                              cb1 = WIDGET_BUTTON(contextBase1, VALUE = 'browse',  EVENT_PRO = 'getDir')
              FileListLabel=Widget_label(base2,/Align_left, value="Files:",uname='Files',uvalue=CREATE_STRUCT('name1',['']))
              FileList=WIDGET_LIST(base2,event_pro="selFile",ysize=6,UNAME = 'FileList',value=Files.disp, uvalue=Files.path,/multiple)
              PTRlabel=Widget_label(base2,/Align_left, value=' Specify PTR-MS data:')
              base4=widget_base(base2,/row,xsize=200,ysize=320)
                      base5=widget_base(base4,/column,xsize=100,ysize=200)
                              PTRlabel=Widget_label(base5,/Align_right,uname='Label1', value='p-drift',ysize=22)
                              PTRlabel=Widget_label(base5,/Align_right, value='U-drift',ysize=22)
                              PTRlabel=Widget_label(base5,/Align_right, value='Udx',ysize=22)
                              PTRlabel=Widget_label(base5,/Align_right, value='T-drift',ysize=22)
                              EngDataListLabel=Widget_label(base5,/Align_left, value="Engineering:", uname='Engineering', uvalue=0)
                              EngDataList=WIDGET_LIST(base5,event_pro="selEng", value=engnames, ysize=15,UNAME = 'EngList',uvalue=create_struct('data', -9999,'names',Engnames))
                      base6=widget_base(base4,/column,xsize=95,ysize=200)
                            ; PTRdrop1=widget_droplist(base6,uname='PTRdrop1',value=['  ',transpose(engnames)])
                              PTRdrop1=widget_droplist(base6,uname='PTRdrop1',VALUE= ['  ',transpose(engnames)],xsize=85)
                              PTRdrop2=widget_droplist(base6,uname='PTRdrop2',VALUE= ['  ',transpose(engnames)],xsize=85)
                              PTRdrop3=widget_droplist(base6,uname='PTRdrop3',VALUE= ['  ',transpose(engnames)],xsize=85)
                              PTRdrop4=widget_droplist(base6,uname='PTRdrop4',VALUE= ['  ',transpose(engnames)],xsize=85)
                              MassListLabel=Widget_label(base6,/Align_left, value="Masses:", uname="Masses",uvalue=59.048)
                              MassList=WIDGET_LIST(base6,event_pro="selMass", ysize=14,xsize=11,UNAME = 'MassList',value=string(masses,format='(F9.3)'),uvalue=masses)
                
               UnifiedMassListLabel=Widget_label(base2,/Align_left, value="Data export")
              
              label3=Widget_label(base2,/Align_left, value="Index-file:",uname='Index',uvalue=-9999)
              wText2 = WIDGET_TEXT(base2, VALUE='click  right mouse button to select a index-file', uname="textfield2", /EDITABLE,/ALL_EVENTS, /NO_NEWLINE,/wrap , /CONTEXT_EVENTS, YSIZE = 2,uvalue=-9999)
                      contextBase2 = WIDGET_BASE(wText2,  /CONTEXT_MENU,  UNAME="contextMenu2")
                              cb2 = WIDGET_BUTTON(contextBase2, VALUE = 'browse',  EVENT_PRO = 'SelectIndexFile')
              button4 = WIDGET_BUTTON(base2,event_pro='NewIndex', value='Create New Index File',uname='NewIndex') 
              button3 = WIDGET_BUTTON(base2, event_pro='exprt',value='Export',uname='exprt',uvalue=CREATE_STRUCT('PeakLoc',[-9999])) 
  
    base10=widget_base(base2,/row,uname='base_10',xsize=200,ysize=24)
                      Label_9=Widget_label(base10,/Align_left,  value="File number (first-last)")
                      Text_Ind1 = WIDGET_TEXT(base10, uname='ExpStrt', VALUE='0',/EDITABLE,xsize=40, scr_xsize=40)
                      Text_Ind2 = WIDGET_TEXT(base10, uname='ExpEnd', VALUE='0',/EDITABLE,xsize=40, scr_xsize=40)
           
  
   
  WIDGET_CONTROL, base, /REALIZE
  XMANAGER, 'ProcPTR', base, /NO_BLOCK
  
END


pro dropsel, event
 WIDGET_CONTROL, event.id+1, gET_VALUE= sel
  sel=sel+strtrim(string(WIDGET_INFO(event.id, /droplist_select)-1),2)+','
  WIDGET_CONTROL, event.id+1, sET_VALUE= sel
 
end

pro exprt, event
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), gET_uVALUE=lib
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), gET_VALUE= path
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'FileList'), GET_uVALUE=Files
   ; WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), gET_uVALUE= Engnames
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield2'), gET_VALUE= IndexFile
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'ExpStrt'), gET_VALUE=i_start
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'ExpEnd'), gET_VALUE=i_end
    name=strmid(indexfile,strlen(path)+1,strlen(indexfile)-strlen(path)-5)
    
   
    i_pdrift=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop1'), /droplist_select)-1  
    i_udrift=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop2'), /droplist_select)-1  
    i_udx=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop3'), /droplist_select)-1  
    i_Tdrift=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop4'), /droplist_select)-1  
    start=max(float(i_start))
    ende=max(float(i_end))
    i=start
    ;Engnames=reform(Engnames.names)
    ppbdata=readfloat(files[i])
    cpsdata=readfloat(strmid(files[i],0,strpos(files[i],'ppb')-4)+'cps\OCorr\corrcps'+strmid(files[i],strpos(files[i],'ppb')+3,20)+'UL.fdt')
    engnames=reform(readcsvstr(strmid(files[i[0]],0,strpos(files[i[0]],'ppb')-7)+'EngData\EngDataNames'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'.csv'))
    engdata=readfloat(strmid(files[i],0,strpos(files[i],'ppb')-7)+'EngData\EngData'+ strmid(files[i],strpos(files[i],'ppb')+3,20)+'.fdt')
    masses2=  readcsv(strmid(files[i],0,strpos(files[i],'ppb')-4)+'IonList\MassIDs_'+strmid(files[i],strpos(files[i],'ppb')+3,20)+'UL.csv')
    par=readfloat(strmid(files[i],0,strpos(files[i],'ppb')-14)+'FileInfo\Time2MassPAR\'+strmid(files[i],strpos(files[i],'ppb')+3,20)+'PAR2.fdt')
    mode=par[9]
 
    ind=index(ppbdata, masses2,engnames,engdata,indexfile)
    S1=averaging(cpsdata,masses2,engdata,engnames,ind)   
    FinalDataNames=S1.names
    carryover=S1.carryover
    FiDaNa=float(FinalDataNames)
    
    FinalData=S1.data  
    FinalDataErr=S1.dataErr  
    engrest=S1.resteng 
    datarest=S1.restdata
    if (max(datarest) gt -19999) then begin
            if(size(datarest,/n_dimensions) eq 1)then restcyc=[-9999,1] else restcyc=size(datarest,/dimensions) 
            restcyc=restcyc[1]
            cyc=size(ppbdata,/dimensions) & cyc=cyc[1]
            datarestppb=ppbdata[*,indgen(restcyc)+cyc-restcyc]
    endif else datarestppb=-19999
    
 ;   m21=FinalData[*,where(abs(FiDaNa-21.021) le 0.002)] & m21=m21[*,0]
  ;  if(getpar('m38') eq 1) then begin
   ;         m38=FinalData[*,where(abs(FiDaNa-38.0326) le 0.002)]
    ;        m38=m38[*,0]
    ;endif else begin
     ;       m38=FinalData[*,where(abs(FiDaNa-39.0327) le 0.002)]
      ;      m38=m38[*,0]
 
   ; endelse
    pdrift=FinalData[*, where(strmatch(FinalDataNames,'p_drift*') eq 1 or strmatch(FinalDataNames,'p-Drift*') eq 1)] 
    mistUD=FinalData[*, where(strmatch(FinalDataNames,'Udrift*') eq 1)]
    mistUD=mistUD[*,0]
    udrift=mistUD+FinalData[*,where(strmatch(FinalDataNames,'Uq*') eq 1 or strmatch(FinalDataNames,'Udx*') eq 1)]
    tdrift=FinalData[*,where(strmatch(FinalDataNames,'Drift_Temperature*') eq 1 or strmatch(FinalDataNames,'T-Drift*') eq 1)]+273.15
    pdrift=pdrift[*,0]
    udrift=udrift[*,0]
    tdrift=tdrift[*,0]
    masses=reform(FiDaNa[where(FiDaNa gt 0)])
    
    
    Prim=getPrimIons(masses,transpose(FinalData[*,where(FiDaNa gt 0)]),mode)
    FinalData[*,where(FiDaNa gt 0)]=transpose(calcppb( transpose(FinalData[*,where(FiDaNa gt 0)]), masses, Prim.A,Prim.B,pdrift,udrift,tdrift))
    
    
    
    print, string(i)+'  '+files[i]
                
    ;for i=1,max(size(files,/dimensions))-1 do begin
   for i=start+1,ende do begin
          ppb=readfloat(files[i])
          cps=readfloat(strmid(files[i],0,strpos(files[i],'ppb')-4)+'cps\OCorr\corrcps'+strmid(files[i],strpos(files[i],'ppb')+3,20)+'UL.fdt')
          eng=readfloat(strmid(files[i],0,strpos(files[i],'ppb')-7)+'EngData\EngData'+strmid(files[i],strpos(files[i],'ppb')+3,20)+'.fdt')
          if (max(cps) gt -9999) then if (max(eng) gt -9999) then begin
                  if (max(datarestppb) gt -19999) then ppbdata=[[datarestppb],[ppb]] else ppbdata=ppb
                  if (max(datarest) gt -19999) then cpsdata=[[datarest],[cps]] else cpsdata=cps
                  if (max(engrest) gt -19999) then  engdata=[engrest,eng] else engdata=eng
                  ind=index(ppbdata, masses2,engnames,engdata,indexfile)
                  S1=averaging(cpsdata,masses2,engdata,engnames,ind,carryover)  
                  engrest=S1.resteng 
                  datarest=S1.restdata 
                  if (max(datarest) gt -19999) then begin
                            if(size(datarest,/n_dimensions) eq 1) then restcyc=[-9999,1] else restcyc=size(datarest,/dimensions) 
                            restcyc=restcyc[1]
                            cyc=size(ppbdata,/dimensions) & cyc=cyc[1]
                            datarestppb=ppbdata[*,indgen(restcyc)+cyc-restcyc]
                  endif else datarestppb=-19999
                  newdat=S1.data  
                  newdatErr=S1.dataErr  
    ;              m21=newdat[*,where(abs(FiDaNa-21.021) le 0.002)] & m21=m21[*,0]
     ;             if(getpar('m38') eq 1) then begin
      ;                    m38=newdat[*,where(abs(FiDaNa-38.0326) le 0.002)]
       ;                   m38=m38[*,0]
        ;          endif else begin
         ;                 m38=newdat[*,where(abs(FiDaNa-39.0327) le 0.002)]
          ;                m38=m38[*,0]
           ;       endelse
   
                  pdrift=newdat[*, where(strpos(FinalDataNames,Engnames[i_pdrift]) gt -1)] 
                  udrift=newdat[*, where(strpos(FinalDataNames,Engnames[i_udrift]) gt -1)]+newdat[*,where(strpos(FinalDataNames,Engnames[i_udx]) gt -1)]    
                  tdrift=newdat[*,where(strpos(FinalDataNames,Engnames[i_Tdrift]) gt -1)]+273.15
                  pdrift=pdrift[*,0]
                  udrift=udrift[*,0]
                  tdrift=tdrift[*,0]
                  masses=reform(FiDaNa[where(FiDaNa gt 0)])
                  Prim=getPrimIons(masses, transpose(newdat[*,where(FiDaNa gt 0)]),mode)
                  newdat[*,where(FiDaNa gt 0)]=transpose(calcppb( transpose(newdat[*,where(FiDaNa gt 0)]), masses, Prim.A,Prim.B,pdrift,udrift,tdrift))
                  FinalData=[FinalData,newdat]
                  FinalDataErr=[FinalDataErr,newdatErr]
                  print, string(i)+'  '+files[i]
          endif else print, 'NO DATA: '+string(i)+'  '+files[i]
    endfor
    makefloat, path+'Averaged_'+name+'_'+strtrim(string(start,Format='(I5)'),2)+'_'+strtrim(string(ende,Format='(I5)'),2)+'.fdt', FinalData
    makefloat,path+'AveragedErr_'+name+'_'+strtrim(string(start,Format='(I5)'),2)+'_'+strtrim(string(ende,Format='(I5)'),2)+'.fdt', FinalDataErr
        if(getpar('SaveCsv') eq 1) then  MakeCsv,path+'Averaged_'+name+'_'+strtrim(string(start,Format='(I5)'),2)+'_'+strtrim(string(ende,Format='(I5)'),2)+'.csv',transpose([transpose(FinalDataNames),string(FinalData)])
       if(getpar('SaveCsv') eq 1) then  MakeCsv,path+'AveragedErr_'+name+'_'+strtrim(string(start,Format='(I5)'),2)+'_'+strtrim(string(ende,Format='(I5)'),2)+'.csv',transpose([transpose(FinalDataNames),string(FinalDataErr)])
   
    makecsv,path+'AveragedNames_'+name+'_'+strtrim(string(start,Format='(I5)'),2)+'_'+strtrim(string(ende,Format='(I5)'),2)+'.csv',transpose(FinalDataNames)
    print, 'exprt'
end


PRO getDir, event
  COMPILE_OPT hidden
  Path=''
  Path=dialog_pickfile( TITLE='Select directory (ppb data)', /DIRECTORY )
  if (path NE '') then begin
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), SET_VALUE= path
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'FileList'), SET_VALUE= ''
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), SET_VALUE= ''
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'MassList'), SET_VALUE= ''
          Files=Filelst(path)
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'FileList'), SET_VALUE= Files.disp
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'FileList'), SET_uVALUE= Files.path
          masses=readcsv(path+'UnifiedMassList.csv')
          masses=masses[*,0]
          engnames=readcsvstr(path+'Export/EngData/'+'Engdatanames.csv')
          
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), SET_VALUE= Engnames
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), SET_uVALUE= create_struct('data', -9999,'names',Engnames)
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'MassList'), SET_VALUE= string(masses,format='(F9.3)')
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'MassList'), SET_uVALUE= masses
          
      
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop1'), SET_VALUE= ['  ',transpose(engnames)]
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop2'), SET_VALUE= ['  ',transpose(engnames)]
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop3'), SET_VALUE= ['  ',transpose(engnames)]
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop4'), SET_VALUE= ['  ',transpose(engnames)]
                          
  endif
END



pro NewIndex,event
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), gET_VALUE= path
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield2'), gET_VALUE= IndexFile
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'FileList'), GET_uVALUE=Files
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), GET_uVALUE=S1
   i=widget_info(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'FileList'),/List_select)
  
 engnames=readcsvstr(strmid(files[i[0]],0,strpos(files[i[0]],'ppb')-7)+'EngData/EngDataNames'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'.csv')
   
  
  
    CondOps= ['         ','    <    ','    =    ','    >    ','AND  =','OR =','AND NOT =','OR NOT =','subtract  >']

     base=widget_base(/column,xsize=510,ysize=715)
         base0=widget_base(base,/row,xsize=500,ysize=26)
              labe=Widget_label(base0,/Align_left, value='', xSIZE = 20)
              Text0 = WIDGET_TEXT(base0, VALUE='Index_Name', uname='Text0', /EDITABLE)
              but = WIDGET_BUTTON(base0,event_pro='SetIndVal',value='get current values', uname='but',uvalue=IndexFile) 
              labe=Widget_label(base0,/Align_left, value='               max averaging interval [s]:', xSIZE = 170)
              Text0 = WIDGET_TEXT(base0, VALUE='3600', uname='Textt0', xSIZE = 7, /EDITABLE)
                       
   labe=Widget_label(base,/Align_left, value='  INDEX_M (measurements):   ', xSIZE = 500) 
   base0=widget_base(base,/row,xsize=500,ysize=18)
   labe=Widget_label(base0,/Align_left, value='Select Engineering Data ', xSIZE = 120) 
   labe=Widget_label(base0,/Align_left, value='| Selection ', xSIZE = 90) 
   labe=Widget_label(base0,/Align_left, value=' OPs & COND', xSIZE = 80) 
    labe=Widget_label(base0,/Align_left, value=' Value', xSIZE = 70) 
   labe=Widget_label(base0,/Align_left, value='indM value', xSIZE = 70) 
   labe=Widget_label(base0,/Align_left, value=' default', xSIZE = 70) 
   sch=['0','100','200']
   for i=1,10 do begin
          if (i lt 9.5) then ii=string(i,format='(I1)') else ii=string(i,format='(I2)')
          base1=widget_base(base,/row,xsize=500,ysize=24)
          Drop1=widget_droplist(base1, event_pro='DropSel',uname='Drop'+ii,value=['  ',transpose(engnames)], xSIZE = 120,uvalue=path)
          Tex1 = WIDGET_TEXT(base1, VALUE='', uname='Tex'+ii, /EDITABLE, YSIZE = 1, xSIZE = 12)
          Dropp1=widget_droplist(base1,uname='Dropp'+ii,value=CondOps, xSIZE = 80)
          Text1 = WIDGET_TEXT(base1, VALUE='', uname='Text'+ii, /EDITABLE, YSIZE = 1, xSIZE = 7)
          label1=Widget_label(base1,/Align_left, value='', xSIZE = 20)
          if(i lt 2.5) then Textt1 = WIDGET_TEXT(base1, VALUE=sch[i], uname='Textt'+ii, YSIZE = 1, xSIZE = 7)
          if(i gt 2.5) then Textt1 = WIDGET_TEXT(base1, VALUE='', uname='Textt'+ii, /EDITABLE, YSIZE = 1, xSIZE = 7)
          labell1=Widget_label(base1,/Align_left, value='', xSIZE = 15)
          if(i ne 1) then labell1=Widget_label(base1,/Align_left, value='', xSIZE = 70)
          if(i eq 1) then Texttt1 = WIDGET_TEXT(base1, VALUE='300', uname='Texttt'+ii,  YSIZE = 1, xSIZE = 7) else Texttt1 = WIDGET_TEXT(base1,/Align_left, value=' ', uname='Texttt'+ii)
   endfor
       
   
            
         lab=Widget_label(base,/Align_left, value='Hardcopy Section: Specify INDEX value onto which VALUES are added pointwize ', xSIZE = 500)
        base0=widget_base(base,/row,xsize=500,ysize=18)
        labe=Widget_label(base0,/Align_left, value='INDEX ', xSIZE = 60) 
        labe=Widget_label(base0,/Align_left, value='|         VALUES ', xSIZE = 200) 
       
         base15=widget_base(base,/row,xsize=500,ysize=30)
              Text15 = WIDGET_TEXT(base15, VALUE='', uname='Dext15',/Wrap, /EDITABLE, YSIZE = 2, xSIZE = 8)
              Textt15 = WIDGET_TEXT(base15, VALUE='', uname='Dextt15',/Wrap, /EDITABLE, YSIZE = 2, xSIZE = 70)
      
         base16=widget_base(base,/row,xsize=500,ysize=30)
              Text16 = WIDGET_TEXT(base16, VALUE='', uname='Dext16',/Wrap, /EDITABLE, YSIZE = 2, xSIZE = 8)
              Textt16 = WIDGET_TEXT(base16, VALUE='', uname='Dextt16',/Wrap, /EDITABLE, YSIZE = 2, xSIZE = 70)
         
        base17=widget_base(base,/row,xsize=500,ysize=30)
              Text17 = WIDGET_TEXT(base17, VALUE='', uname='Dext17',/Wrap, /EDITABLE, YSIZE = 2, xSIZE = 8)
              Textt17 = WIDGET_TEXT(base17, VALUE='', uname='Dextt17',/Wrap, /EDITABLE, YSIZE = 2, xSIZE = 70)
         
        base18=widget_base(base,/row,xsize=500,ysize=30)
              Text18 = WIDGET_TEXT(base18, VALUE='', uname='Dext18',/Wrap, /EDITABLE, YSIZE = 2, xSIZE = 8)
              Textt18 = WIDGET_TEXT(base18, VALUE='', uname='Dextt18',/Wrap, /EDITABLE, YSIZE = 2, xSIZE = 70)
     
      labe=Widget_label(base,/Align_left, value='  INDEX_S (sampling):   ', xSIZE = 500) 
   base0=widget_base(base,/row,xsize=500,ysize=18)
   labe=Widget_label(base0,/Align_left, value='Select Engineering Data ', xSIZE = 120) 
   labe=Widget_label(base0,/Align_left, value='| Selection ', xSIZE = 90) 
   labe=Widget_label(base0,/Align_left, value=' OPs & COND', xSIZE = 80) 
    labe=Widget_label(base0,/Align_left, value=' Value', xSIZE = 70) 
   labe=Widget_label(base0,/Align_left, value='indS value', xSIZE = 70) 
   labe=Widget_label(base0,/Align_left, value='ass indM', xSIZE = 70) 
  
   for i=11,16 do begin
          if (i lt 9.5) then ii=string(i,format='(I1)') else ii=string(i,format='(I2)')
          base1=widget_base(base,/row,xsize=500,ysize=24)
          Drop1=widget_droplist(base1, event_pro='DropSel',uname='Drop'+ii,value=['  ',transpose(engnames)], xSIZE = 120,uvalue=path)
          Tex1 = WIDGET_TEXT(base1, VALUE='', uname='Tex'+ii, /EDITABLE, YSIZE = 1, xSIZE = 12)
          Dropp1=widget_droplist(base1,uname='Dropp'+ii,value=CondOps, xSIZE = 80)
          Text1 = WIDGET_TEXT(base1, VALUE='', uname='Text'+ii, /EDITABLE, YSIZE = 1, xSIZE = 7)
          label1=Widget_label(base1,/Align_left, value='', xSIZE = 20)
           if(i eq 11 or i eq 13 or i eq 15) then Textt1 = WIDGET_label(base1,/Align_left, VALUE='10', uname='Textt'+ii,  xSIZE = 50)
           if(i eq 12 or i eq 14 or i eq 16) then Textt1 = WIDGET_label(base1,/Align_left, VALUE='20', uname='Textt'+ii,  xSIZE = 50)
           label1=Widget_label(base1,/Align_left, value='', xSIZE = 15)
          if(i eq 12 or i eq 14 or i eq 16) then labell1=Widget_label(base1,/Align_left, value='', xSIZE = 70)
          if(i eq 11 or i eq 13 or i eq 15) then Texttt1 = WIDGET_TEXT(base1, VALUE='', uname='Texttt'+ii, /EDITABLE, YSIZE = 1, xSIZE = 7) else Texttt1 = WIDGET_TEXT(base1,/Align_left, value='', uname='Texttt'+ii) 
   endfor
      
     
         
         button = WIDGET_BUTTON(base,event_pro='SaveIndex', value='Save index File',uname='SaveIndex') 
         
            
  WIDGET_CONTROL, base, /REALIZE
  XMANAGER, 'NewIndex', base, /NO_BLOCK    
   
end

PRO NewIndex_event, event
 
END


pro plt, event

    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), gET_uVALUE=Seng
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Files'), gET_uVALUE=Sppb
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Engineering'), get_uVALUE=xi
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Masses'), get_uVALUE= mass
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), gET_uVALUE=lib
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield2'), gET_VALUE=indexfile
    i_pdrift=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop1'), /droplist_select)-1  
    i_udrift=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop2'), /droplist_select)-1  
    i_udx=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop3'), /droplist_select)-1  
    i_Tdrift=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop4'), /droplist_select)-1  
     WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label1'), gET_uVALUE=par
      mode=par[9]
       
    ppbdata=Sppb.data
    cpsdata=Sppb.cpsdata
    masses2=Sppb.names
    engdata=Seng.data
    engnames=Seng.names
    ind=index(ppbdata, masses2,engnames,engdata,indexfile)
    indexM=ind.m
    indexS1=ind.s1 
    indexS2=ind.s2 
    indexS3=ind.s3 
    if (ind.T ne -9999) then begin
    S1=averaging(ppbdata,masses2,engdata,engnames,ind)    
    FinalDataNames=S1.names
    FiDaNa=float(FinalDataNames)
    FinalData=S1.data  
    FinalDataRow=FinalData[*,where(abs(FiDaNa-mass) le 0.002)]
    S1=averaging(cpsdata,masses2,engdata,engnames,ind)    
    FinalCpsData=S1.data  
    FinalCpsDataErr=S1.dataErr  
    FinalCpsDataRow=FinalCpsData[*,where(abs(FiDaNa-mass) le 0.002)]
    FinalCpsDataRow2Err=FinalCpsDataErr[*,where(abs(FiDaNa-mass) le 0.002)]
;    m21=FinalCpsData[*,where(abs(FiDaNa-21.021) le 0.002)] & m21=m21[*,0]
 ;   if(getpar('m38') eq 1) then begin
  ;          m38=FinalCpsData[*,where(abs(FiDaNa-38.0326) le 0.002)]
   ;         m38=m38[*,0]
    ;endif else begin
     ;       m38=FinalCpsData[*,where(abs(FiDaNa-39.0327) le 0.002)]
      ;      m38=m38[*,0]
   ; endelse
    pdrift=FinalCpsData[*, where(FinalDataNames eq Engnames[i_pdrift])] 
    udrift=FinalCpsData[*, where(FinalDataNames eq Engnames[i_udrift])]+FinalCpsData[*,where(FinalDataNames eq Engnames[i_udx])]    
    tdrift=FinalCpsData[*,where(FinalDataNames eq Engnames[i_Tdrift])]+273.15
    pdrift=pdrift[*,0]
    udrift=udrift[*,0]
    tdrift=tdrift[*,0]
    Prim=getPrimIons(FiDaNa[where(FiDaNa ne 0)],transpose(FinalCpsData[*,where(FiDaNa ne 0)]),mode)
    FinalDataRow2=calcppb( FinalCpsDataRow, mass, Prim.A,Prim.B,pdrift,udrift,tdrift)
    endif
    
    
    k=min(where(abs(masses2-mass) lt 0.002))
    x=-9999
    y=-9999
    if(k gt -0.5) then begin 
          x=engdata[*,0]
          y=ppbdata[k,*]
          y2=y
          ytit='nmol/mol'
    endif
    if( event.id eq  WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList')) then  begin
          x=engdata[*,0]
          y=engdata[*,xi]
          y2=ppbdata[k,*]
          ranY=range(y[where(y gt -9999)])
          ranY2=range(y2[where(y2 gt -9999)])
          y2=y2 /( (ranY2[1]-ranY2[0])/(ranY[1]-ranY[0]))
          ranY2=range(y2[where(y2 gt -9999)])
          y2=y2-(ranY2[0]-ranY[0])
         ytit=' '
    endif
    caldat, min(x)+ Julday(1,1,2009,0,0,0), Month,Day,Year, Hour,Minute
    time1=string(Day,format='(I2.2)')+'.'+string(Month,format='(I2.2)')+'.'+string(Year,format='(I4.4)')+', '+string(Hour,format='(I2.2)')+':'+string(Minute,format='(I2.2)')
    caldat, max(x)+ Julday(1,1,2009,0,0,0), Month,Day,Year, Hour,Minute
    time2=string(Day,format='(I2.2)')+'.'+string(Month,format='(I2.2)')+'.'+string(Year,format='(I4.4)')+', '+string(Hour,format='(I2.2)')+':'+string(Minute,format='(I2.2)')
    misstx=max(x)-min(x)   
    xran=[min(x)-misstx*0.05,max(x)+misstx*0.05]
    misst=max(y[ where(y gt -9999) ])-min(y[ where(y gt -9999) ]) 
    yran=[min(y[ where(y gt -9999) ])-misst*0.1,max(y[ where(y gt -9999) ])+misst*0.2]
  
   DEVICE, DECOMPOSED = 0 
   !P.MULTI = [0,1,1] 
   loadct,30
   plot, x[where(y gt -9999)],  y[where(y gt -9999)], /YNOZERO, ytitle=ytit,xrange=xran,yrange=yran,xstyle=1,ystyle=1, color=0, background=-1, thick=1,charsize=1.5,charthick=2
   loadct,31
   oplot, x,  y2,  color=0,thick=1 ;oplot all raw data in red
   oplot, x,  y,  color=12,thick=1 ;oplot all raw data in red
   xyouts,0.15,0.87 , 'm/z = '+strtrim(string(mass,format='(F9.3)'),2), color=16, charthick=3, charsize=1.5,/normal
   xyouts,0.15,0.91 , time1+' to '+time2, color=16, charthick=3, charsize=1.5,/normal
   if (ind.T ne -9999) then begin
      indvalues=multiples(indexM[sort(indexM)]) & indvalues=indvalues[*,0]
      coltab=[1,3,8]
      IS=[[indexS1],[indexS2],[indexS3]]
      for gg=0,2 do begin    
          filt2=where(indvalues ge 100+gg*100 and indvalues lt 150+gg*100)
          if(max(filt2) gt -0.5) then begin
                  indvaluesS1=indvalues[filt2]
                  nS1=max(size(indvaluesS1,/dimensions))
                  colorsS1=70+indgen(nS1)*floor(150/nS1)
                  for n=0,nS1-1 do begin ;oplot indM 100 & indS1
                          loadct,coltab[gg]
                          filt1=where(indexM eq indvaluesS1[n])
                          if(max(filt1) gt -1) then  oplot, x[filt1],  y[filt1], psym=4,symsize=1, color=colorsS1[n],  thick=3
                          sch2=reform(IS[*,gg])
                          if(max(where(sch2 gt 0)) gt -0.5) then oplot, x[where(sch2 gt 0)],min(yran)+misst*gg*0.005+sch2[where(sch2 gt 0)]*misst*0.1/(30),color=170, thick=1, psym=4
                          xyouts,0.6+0.1*gg,0.91-0.025*n , 'ind='+strtrim(string(indvaluesS1[n]),2), color=colorsS1[n], charthick=3, charsize=1.5,/normal
                  endfor
          endif       
       endfor
       loadct,31
          for j=0,max(size(FinalDataRow,/dimensions))-1 do begin ; averaged values from cps
                  oplot, [FinalData[j,0],FinalData[j,1]],[FinalDataRow2[j],FinalDataRow2[j]], thick=8, color=-1 
                  err=abs(FinalDataRow2[j]*FinalCpsDataRow2Err[j])
                  errplot,(FinalData[j,0]+FinalData[j,1])/2 ,FinalDataRow2[j]-err,FinalDataRow2[j]+err, color=-1,  thick=8
          endfor
          loadct,31
          for j=0,max(size(FinalDataRow,/dimensions))-1 do begin ; averaged values from ppb
                  oplot, [FinalData[j,0],FinalData[j,1]],[FinalDataRow[j],FinalDataRow[j]], thick=5, color=187 
          endfor
          xyouts,0.15,0.125,'___',color=187, charthick=4, charsize=1.5,/normal
          xyouts,0.18,0.12,'VMR from ppb files',color=187, charthick=3, charsize=1.5,/normal
          for j=0,max(size(FinalDataRow,/dimensions))-1 do begin ; averaged values from cps
                  oplot, [FinalData[j,0],FinalData[j,1]],[FinalDataRow2[j],FinalDataRow2[j]], thick=5, color=16 
                  err=abs(FinalDataRow2[j]*FinalCpsDataRow2Err[j])
                  errplot,(FinalData[j,0]+FinalData[j,1])/2 ,FinalDataRow2[j]-err,FinalDataRow2[j]+err, color=16,  thick=5
          endfor
          xyouts,0.5,0.125,'___',color=16, charthick=4, charsize=1.5,/normal
          xyouts,0.53,0.12,'VMR calculated from averaged cps values',color=16, charthick=3, charsize=1.5,/normal
   endif
  
end



PRO ProcPTR_event, event
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_CONTEXT') THEN BEGIN
   ; contextBase = WIDGET_INFO(event.ID, FIND_BY_UNAME = 'contextMenu')
  ;    contextBase2 = WIDGET_INFO(event.ID, FIND_BY_UNAME = 'contextMenu2')
   ;  WIDGET_DISPLAYCONTEXTMENU, event.ID, event.X, event.Y, contextBase
  
    WIDGET_DISPLAYCONTEXTMENU, event.ID, event.X, event.Y, event.ID+1
 
  ENDIF

END

pro SaveIndex,event

WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text0'), gET_VALUE= name
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Textt0'), gET_VALUE= interval
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Drop1'), gET_uVALUE= path
if(str2vec(interval) eq -9999) then interval='3600'
IndexPar='_Textt0='+strtrim(interval,2)

for i=1,16 do begin
      if (i lt 9.5) then ii=string(i,format='(I1)') else ii=string(i,format='(I2)')
     ; vector=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Drop'+ii), /droplist_select)
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Tex'+ii), gET_VALUE=vecSel
      condition=WIDGET_INFO(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dropp'+ii), /droplist_select)
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text'+ii), gET_VALUE=wert
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Textt'+ii), gET_VALUE=INDif
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Texttt'+ii), gET_VALUE=INDelse
      IndexPar=[IndexPar,'_Tex'+ii+'='+strtrim(vecSel,2),'_Dropp'+ii+'='+strtrim(string(condition,format='(I2)'),2),    $
      '_Text'+ii+'='+strtrim(wert,2),'_Textt'+ii+'='+strtrim(INDif,2),'_Texttt'+ii+'='+strtrim(INDelse,2)]
endfor   

for i=15,18 do begin
      ii=string(i,format='(I2)') 
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dext'+ii), gET_VALUE=IndValue & IndValue=strjoin(strtrim(IndValue,2),/single)
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dextt'+ii), gET_VALUE=Values2add & Values2add=strjoin(strtrim(Values2add,2),/single)
      IndexPar=[   IndexPar,'_Dext'+ii+'='+IndValue,'_Dextt'+ii+'='+Values2add] 
endfor  




makecsv,path+name+'.ind',transpose(IndexPar)
        


       WIDGET_CONTROL, event.TOP, /DESTROY
       
 
    
end
 

PRO SelectIndexFile, event
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), gET_VALUE= path
   
  COMPILE_OPT hidden
  IndexFile=''
  IndexFile=dialog_pickfile( TITLE='Select an index-file (*.ind)',Filter='*.ind', path=path, /MUST_EXIST )
  
  if (IndexFile NE '') then begin
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield2'), SET_VALUE= IndexFile
     
     
      
      S1=ReadIndexFile(IndexFile) 
    
  
       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield2'), SET_uVALUE= S1
     
  endif
  end


pro selEng, event
    xi=event.index
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Engineering'), set_uVALUE=xi
    plt, event
end


PRO selFile, event
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'FileList'), GET_uVALUE=Files
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), GET_uVALUE=S1
   i=widget_info(WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'FileList'),/List_select)
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'ExpStrt'), sET_VALUE=string(min(i),format='(I4)')
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'ExpEnd'), sET_VALUE=string(max(i),format='(I4)')
  
  
  engnames=readcsvstr(strmid(files[i[0]],0,strpos(files[i[0]],'ppb')-7)+'EngData\EngDataNames'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'.csv')
  par=readfloat(strmid(files[i[0]],0,strpos(files[i[0]],'ppb')-14)+'FileInfo\Time2MassPAR\'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'PAR2.fdt')
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label1'), sET_uVALUE=par
  
 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), sET_VALUE=engnames
 
   dr1=WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop1')
   dr2=WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop2')
   dr3=WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop3')
   dr4=WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'PTRdrop4')
  
  set1=WIDGET_INFO(dr1, /droplist_select) 
  set2=WIDGET_INFO(dr2, /droplist_select) 
  set3=WIDGET_INFO(dr3, /droplist_select) 
  set4=WIDGET_INFO(dr4, /droplist_select) 
  
   WIDGET_CONTROL, dr1, sET_VALUE= ['  ',transpose(engnames)]
   WIDGET_CONTROL, dr2, sET_VALUE= ['  ',transpose(engnames)]
   WIDGET_CONTROL, dr3, sET_VALUE= ['  ',transpose(engnames)]
   WIDGET_CONTROL, dr4, sET_VALUE= ['  ',transpose(engnames)]
  
  WIDGET_CONTROL, dr1, SET_DROPLIST_SELECT=set1
  WIDGET_CONTROL, dr2, SET_DROPLIST_SELECT=set2
  WIDGET_CONTROL, dr3, SET_DROPLIST_SELECT=set3
  WIDGET_CONTROL, dr4, SET_DROPLIST_SELECT=set4
 
  
  
  ;MASSES
  
   ;masses2=readcsv(strmid(files[i[0]],0,strpos(files[i[0]],'ppb'))+'MassIDs_'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'UL.csv')
  masses2=readcsv(strmid(files[i[0]],0,strpos(files[i[0]],'ppb')-4)+'IonList/MassIDs_'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'UL.csv')
   for k=0,max(size(i,/dimensions))-1 do begin
          mist=readcsv(strmid(files[i[k]],0,strpos(files[i[k]],'ppb')-4)+'IonList/MassIDs_'+strmid(files[i[k]],strpos(files[i[k]],'ppb')+3,20)+'UL.csv')
          if(max(size(mist,/dimensions)) lt max(size(masses2,/dimensions))) then masses2=mist
   endfor
  
   ;PPB
   ppb=readfloat(files[i[0]])
   ppb=ppb[indgen(max(size(masses2,/dimensions))),*]
   if (max(size(i,/dimensions)) gt 1) then for k=1,max(size(i,/dimensions))-1 do begin
          mist=readfloat(files[i[k]])
          ppb=[[ppb],[mist[indgen(max(size(masses2,/dimensions))),*]]]
   endfor
  
  
  ;CPS
  ;cpsdata=readfloat(strmid(files[i[0]],0,strpos(files[i[0]],'ppb'))+'cps'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'UL.fdt')
  cpsdata=readfloat(strmid(files[i[0]],0,strpos(files[i[0]],'ppb')-4)+'cps/cps'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'UL.fdt')
  if (max(size(i,/dimensions)) gt 1) then for k=1,max(size(i,/dimensions))-1 do begin
     mist2=readfloat(strmid(files[i[k]],0,strpos(files[i[k]],'ppb')-4)+'cps/cps'+strmid(files[i[k]],strpos(files[i[k]],'ppb')+3,20)+'UL.fdt')
     cpsdata=[[cpsdata],[mist2]]
  endfor
  
  ;ENG
  ;engdata=readfloat(strmid(files[i[0]],0,strpos(files[i[0]],'ppb'))+'EngData'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'.fdt')
  engdata=readfloat(strmid(files[i[0]],0,strpos(files[i[0]],'ppb')-7)+'EngData/EngData'+strmid(files[i[0]],strpos(files[i[0]],'ppb')+3,20)+'.fdt')
  if (max(size(i,/dimensions)) gt 1) then for k=1,max(size(i,/dimensions))-1 do begin
     mist2=readfloat(strmid(files[i[k]],0,strpos(files[i[k]],'ppb')-7)+'EngData/EngData'+strmid(files[i[k]],strpos(files[i[k]],'ppb')+3,20)+'.fdt')
     engdata=[engdata,mist2]
  endfor
   
   eng=create_struct('data',engdata,'names',engnames)
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'EngList'), sET_uVALUE=eng
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Files'), sET_uVALUE=create_struct('data',ppb,'cpsdata',cpsdata,'names',masses2)
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Directory'), sET_uVALUE=masses2
   plt, event
END


pro selMass, event
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'MassList'), get_uVALUE= masses 
    i=event.index
    mass=masses[i] 
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Masses'), set_uVALUE= mass
    plt, event
end


pro SetIndVal, event  

 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'but'), gET_uVALUE= IndexFile
  
  S1=ReadIndexFile(IndexFile) 
    
   for i=1,16 do begin
      if (i lt 9.5) then ii=string(i,format='(I1)') else ii=string(i,format='(I2)')
      helpi=S1.VECTORIND &  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Tex'+ii), SET_VALUE=helpi[i-1]
      helpi=S1.CONDIND &  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dropp'+ii), SET_DROPLIST_SELECT=helpi[i-1]
      helpi=S1.VALUEIND & WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text'+ii), sET_VALUE=helpi[i-1]
      helpi=S1.IFIND & WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Textt'+ii), sET_VALUE=helpi[i-1]
      helpi=S1.ELSEIND &  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Texttt'+ii), sET_VALUE=helpi[i-1]
 
endfor   

for i=15,18 do begin
      ii=string(i,format='(I2)') 
       helpi=S1.INDVALUE & WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dext'+ii), sET_VALUE=helpi[i-15]
      helpi=S1.VALUES2ADD & WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dextt'+ii), sET_VALUE=helpi[i-15]
endfor  

WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Textt0'), sET_VALUE=S1.interval 
       
end
 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;                   Basic data processing
;;;;;;;
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO ____________identification
 end



PRO IdentPTR, event
compile_opt idl2
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Buti1'), GET_uVALUE=path 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Buti2'), GET_uVALUE=lib 
  if (max(lib) eq -9999) then  begin 
          lib=masslib(/extended)
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Buti2'), sET_uVALUE=lib
  endif
 
    log=['']
        
  
  
  
 if(strpos(path,'w_data') lt -0.5) then path='click the right mouse button to select "w_data" directory'
   
   
   base = WIDGET_BASE(/column, XSIZE = 660, YSIZE = 600, RESOURCE_NAME = 'Merge data')
          label1=Widget_label(base,/Align_left, value="Directory:",uname='Directory',uvalue=-9999)
          wText = WIDGET_TEXT(base, VALUE=path, uname="textfield", /EDITABLE,/ALL_EVENTS, /NO_NEWLINE,/wrap , /CONTEXT_EVENTS, YSIZE = 2, uVALUE=lib)
                  contextBase1 = WIDGET_BASE(wText,  /CONTEXT_MENU,  UNAME="contextMenu")
                          cb1 = WIDGET_BUTTON(contextBase1, VALUE = 'browse',  EVENT_PRO = 'getDir2')
          wText2 = WIDGET_TEXT(base, VALUE='click  right mouse button to select a data-file', uname="textfield2", /EDITABLE,/ALL_EVENTS, /NO_NEWLINE,/wrap , /CONTEXT_EVENTS, YSIZE = 2,uvalue=-9999)
                  contextBase2 = WIDGET_BASE(wText2,  /CONTEXT_MENU,  UNAME="contextMenu2")
                           cb2 = WIDGET_BUTTON(contextBase2, VALUE = 'browse',  EVENT_PRO = 'SelectDataFile')
          wText3 = WIDGET_TEXT(base, VALUE='Specify index vamues to be considered (Default is ALL Values)', uname="textfield3", /EDITABLE,/ALL_EVENTS, /NO_NEWLINE,/wrap ,  YSIZE = 1,uvalue=-9999)
          Buti3 = WIDGET_BUTTON(base,event_pro='Identification', value='Attribute Formulas',uname='Buti3') 
          FileListLabel=Widget_label(base,/Align_left, value="Report:",uname='Files',uvalue=CREATE_STRUCT('name1',['']))
          Text_log = WIDGET_TEXT(base, uname="Text_log", VALUE=[log],/ALL_EVENTS, /wrap ,/scroll, YSIZE = 25)
  
   WIDGET_CONTROL, base, /REALIZE
   XMANAGER, 'IdentPTR', base, /NO_BLOCK
 
END



PRO getDir2, event
  COMPILE_OPT hidden
  Path=''
  Path=dialog_pickfile( TITLE='Select directory (ppb data)', /DIRECTORY )
  if (path NE '') then begin
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), SET_VALUE= path
          masses=readcsv(path+'UnifiedMassList.csv')
          masses=masses[*,0]
          engnames=readcsvstr(path+'Export\EngData\'+'Engdatanames.csv')
          
          
      
                              
  endif
END

pro Identification, event
;data, masslist ,sigm, lib


 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), GET_uVALUE=lib 
 
 
 
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), GET_VALUE=path 
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield2'), GET_VALUE=datafile 
 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield3'), GET_VALUE=indMvalues
 
 indmvalues=str2vec(indmvalues)
 names=reform(readcsvstr(path+'AveragedNames_'+ strmid(datafile,strlen(path)+strlen('Averaged_'),strlen(datafile)-strlen(path)-strlen('Averaged_')-3)+'csv'))
 data=readfloat(datafile)
 iM=where(strpos(names,'indM') gt -0.5)
 if(max(iM gt -0.5)) then begin
        indM=data[*,iM]
        filter=-1
        for bbb=0,max(d(indmvalues))-1 do begin 
                ffiill=where(indM eq indmvalues[bbb])
                if(max(ffiill) gt -0.5) then filter=[filter,ffiill]
        endfor
        if(max(filter) gt -1) then data=data[filter,*]
  endif
  masses=float(names)
  data=data[*,where(masses gt 0)]
  names=names[where(masses gt 0)]
  masslist=masses[where(masses gt 0)]
  sigma=readcsv(path+'PeakPar.csv')
  sigma=sigma[*,2]
  sigm=sigma*1e6/masslist ;in ppm
  masslist=float(masslist)
  dims=size(data,/dimensions)
  if(max(size(masslist,/dimensions))ne dims[0]) then data=transpose(data)
  dims=size(data,/dimensions)
  if(max(size(masslist,/dimensions))ne dims[0]) then print, 'MISMATCH MASSLIST and COLUMNS OF DATA (dim[0])'
  len=max(size(masslist,/dimensions))
  report=strarr(8)
  for i=0,len-1 do begin ;loop over masses
  ;for i=119,229 do begin ;loop over masses
        filter=where(data[i,*] gt quantile(data[i,*],0.6) and data[i,*] lt quantile(data[i,*],1)) ; select 40% of data with largest signal
        numbcand=1
        dat1=data[i,filter]
        candidates=match(masslist[i],lib) & candidates=candidates.candidates
        if (max(size(candidates,/n_dimensions)) eq 0) then numbcand=0
        if (max(size(candidates,/n_dimensions)) eq 1) then numbcand=1
        if (max(size(candidates,/n_dimensions)) eq 2) then begin
                  numbcand=d(candidates)
                  numbcand=max(numbcand[1])
        endif
        if(numbcand ge 1) then for j=0,numbcand-1 do begin ; loop over candidates
                labl=' '            
                massCand=masslist[i]+float(candidates[0,j])*0.001 ;mass of the candidate
                libCand=lib[*,where(abs(lib[0,*]-massCand) eq min(abs(lib[0,*]-massCand)))] ; library vector of candidate
                libCand=libCand[1:*]
                dist=isodist(libCand)
                Dmasses=dist.masses; isotope masses
                Ddist=dist.dist; isotope abundances
                ;normalize so that value of current ion (masslist[i]) is 1
                mult=max(1/Ddist[where(abs(Dmasses-masslist[i]) eq min(abs(Dmasses-masslist[i])))])
                Ddist=mult*Ddist  
                len2=max(size(Dmasses,/dimensions))
                for k=0,len2-1 do begin ; loop over isotopes
                        gdnss=-1
                        gdstr='-1'
                        cor=-1
                        len3=0
                        if(abs(Ddist[k]-1) gt 0.00005) then begin ;check isotop distributin in accordance with peak attribution
                                lim1=max([2,150E-3*masslist[i]]); in mDa the larger of 2 and 150ppm
                                ind2=where(abs(masslist-Dmasses[k]) le lim1*0.001) ; the isotope mass...
  if (masslist[i] eq 1137.128) then begin
  print, ''
  endif                             
                               
                               
                                if(max(ind2) lt -0.5) then if(Ddist[k] gt 3) then labl=labl+'No12Cpeak ' else if(Ddist[k] lt 0.4) then  labl=labl+'No13Cpeak ' 
                                if(max(ind2) gt -0.5) then begin 
                                        len3=max(size(ind2,/dimensions))
                                        for m=0,len3-1 do begin
                                                dat2=data[ind2[m],filter]/Ddist[k]
                                                ;dat1 40% largest data of current ion (masslist[i])
                                                ;dat2 40% largest data of isotope peak. NORMALIZED! i.e. that to-be-expected signal on current ion
                                                gdnss2=median(dat2/dat1)
                                                if (gdnss2 lt 0.1) then labl=labl+'<10%' ; less than 10% due to isotope. 
                                                if (gdnss2 ge 0.1 and gdnss2 le 1) then labl=labl+string(100*(gdnss2),format='(I2)')+'%% '; percentage due to istope
                                                if (gdnss2 gt 1) then  labl=labl+'>100%'
                                        endfor
                                endif
                        endif 
;print, '          '+string(len3,format='(i2)')+',  '+string(j,format='(i2)')+',  '+string(k,format='(i2)')+',  '+string(masslist[i],format='(F8.3)')+',  '+ candidates[1,j]+',  '+string(dmasses[k],format='(F8.3)')+',  '+string(ddist[k],format='(F8.3)')+',  '+string(gdnss,format='(F8.3)')+',  '+string(cor,format='(F8.3)')+' | '+gdstr;+',  '+ info
                endfor; loop over isotopes
              ;  datstat=[mean(data[i,*]),quantile(data[i,*],[0.15,0.3,0.5,0.7,0.85])]
                datstat=[mean(data[i,*]),median(data[i,*])]
                labl2='x'
                comm=''
                if(max(strpos(labl,'No12Cpeak')) gt -0.5)then begin
                        comm='DISMISS: no parent peak'
                        labl2='              '
                endif 
                if (strpos(labl2,'x') gt -0.5) then if (max(strpos(labl,'<10%')) gt -0.5) then begin
                        comm='DISMISS: <10%'
                        labl2='  <10%'
                endif
                if (strpos(labl2,'x') gt -0.5) then if (strpos(labl,'%%') gt -0.5) then labl2='     '+strmid(labl,strpos(labl,'%%')-2,2)+'%'
                if (strpos(labl2,'x') gt -0.5) then if (strpos(labl,'>100%') gt -0.5) then labl2='>100%'
                if (strpos(labl2,'x') gt -0.5) then if (strpos(labl,'No13Cpeak') gt -0.5) then begin
                        comm='no isotope peak'
                        labl2='              '
                endif
                if (strpos(labl2,'x') gt -0.5) then comm='No C or N'
                if (strpos(labl2,'x') gt -0.5) then labl2='              '
               formula=candidates[1,j]
                for v=0,16-max(strlen(candidates[1,j])) do formula='   '+formula
         ;      print, string(masslist[i],format='(F8.3)')+'| '+string(sigm[i],format='(I4)')+'| '+string(1e6*float(candidates[0,j])*0.001/masslist[i],format='(I4)')+'| '+labl2+'| '+strjoin(string(datstat,format='(F9.3)'),'|')+'|'+ candidates[1,j]+'| '+comm  ; +strjoin(string(datstat,format='(F9.3)'),'|')    
                report=[[report],[string(masslist[i],format='(F8.3)'),string(sigm[i],format='(I4)'),string(1e6*float(candidates[0,j])*0.001/masslist[i],format='(I4)'),labl2,formula,string(datstat,format='(F9.3)'),comm]]   
                ;print, string(masslist[i],format='(F8.3)')+' '+labl2+' '+labl   
        endfor ; loop over candidates
             
  endfor; loop over masses
report=report[*,1:*]
makecsv,path+'report.csv',transpose([transpose(['m/z ','sigm ',' diff','%max ','formula','mean','median ','comment' ]),transpose(report)])



 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_VALUE=['sigm:     precission of detected m/z (in ppm)','diff:     difference between m/z and MW of formula (in ppm) ','%max:     maximum fraction of signal attributable to formula','mean:     mean of m/z values in dataset','median:     median of  m/z values in dataset','','m/z        sigm  diff   %max       mean      median     formula    comment',strjoin(report,' | ')]

;axe=axe+1
S1=create_struct('report',report)
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_log'), sET_uVALUE=S1

end



PRO IdentPTR_event, event
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_CONTEXT') THEN BEGIN
   
    WIDGET_DISPLAYCONTEXTMENU, event.ID, event.X, event.Y, event.ID+1
 
  ENDIF

end


PRO SelectDataFile, event
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield'), gET_VALUE= path
   
  COMPILE_OPT hidden
  DataFile=''
  DataFile=dialog_pickfile( TITLE='Select an index-file (*.ind)',Filter='*Averaged_*', path=path, /MUST_EXIST )
  
  if (DataFile NE '') then begin
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'textfield2'), SET_VALUE= DataFile
  endif
  end


PRO ____________Filter_samples
 end





PRO FiltPTR, event

 WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Buti1'), GET_uVALUE=path 
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Buti2'), GET_uVALUE=lib 
  
  
  
  
  
  
  
  
  
  uvalFiles=-9999
  valFiles="??? Files in Directory"
  valIons="??? ions in masslist"
 if(strpos(path,'w_data') lt -0.5) then path='click the right mouse button to select "w_data" directory' else begin
 
  Path2=path+'filter/'
   print, "fileinfo"
  mmist=file_info(path2)
  
  if (mmist.directory eq 0) then begin 
          FILE_MKDIR, path2
          FILE_MKDIR, path2+'info'
          FILE_MKDIR, path2+'temp'
 endif
 
 
           Files=Filelst(path)
           
           makecsv, path2+'info/StartTimes.csv',transpose(Files.startTimes)
           uvalFiles=files.path
           valFiles=string(max(size(files.path,/dimensions)),format='(I4)')+' Files in Directory'
         ;  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Files'), SET_uVALUE= files.path
         ;  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Files'), SET_VALUE= string(max(size(files.path,/dimensions)),format='(I4)')+' Files in Directory'
           masslist=readcsv(strjoin([path,'unifiedmasslist.csv']))
           masslist=masslist[*,0]
            n_masses=max(size(masslist,/dimensions))
         ;   masslist=[21.0206,38.0337,masslist[0:n_masses-2]]
       ;    n_masses=max(size(masslist,/dimensions))
       valIons=string(n_masses,format='(I4)')+' ions in masslist'
        ;   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Masses'), SET_VALUE= string(n_masses,format='(I4)')+' ions in masslist'
       
 
  
 
 endelse
   



base = WIDGET_BASE(/column, XSIZE = 230, YSIZE = 735)
   Text_DataDir = WIDGET_TEXT(base, uname="Text_DataDir", VALUE=path, /EDITABLE,/ALL_EVENTS, /NO_NEWLINE,/wrap , /CONTEXT_EVENTS, YSIZE = 3,uvalue=-9999)
       base_3= WIDGET_BASE(Text_DataDir,  /CONTEXT_MENU,  UNAME="base_3")
           But_1 = WIDGET_BUTTON(base_3, uname='But_1', VALUE = 'browse',  EVENT_PRO = 'getDataDirFilt')
   Text_Files = WIDGET_TEXT(base, uname="Text_Files", VALUE=valFiles, /ALL_EVENTS, /NO_NEWLINE, YSIZE = 1,uvalue=uvalFiles)
  Text_Masses = WIDGET_TEXT(base, uname="Text_Masses", VALUE=valIons, /ALL_EVENTS, /NO_NEWLINE, YSIZE = 1,uvalue=-9999)
 
   base2 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 25)
       Lab11 = WIDGET_label(base2,/Align_left, uname="Lab11", VALUE="File ID:  ")
       But_2 = WIDGET_BUTTON(base2, uname='But_2', VALUE = 'previous',  EVENT_PRO = 'pl1')
       Text_2 = WIDGET_TEXT(base2, uname="Text_2", VALUE='-1',  /EDITABLE,/ALL_EVENTS, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       But_3 = WIDGET_BUTTON(base2, uname='But_3', VALUE = '  next  ',  EVENT_PRO = 'pl1')
   base13 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 25)
       Lab13 = WIDGET_label(base13,/Align_left, uname="Lab13", VALUE= "m/z ID: ")
       But_12 = WIDGET_BUTTON(base13, uname='But_12', VALUE = 'previous',  EVENT_PRO = 'pl1')
       Text_12 = WIDGET_TEXT(base13, uname="Text_12", VALUE='-1',  /EDITABLE,/ALL_EVENTS, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       But_13 = WIDGET_BUTTON(base13, uname='But_13', VALUE = '  next  ',  EVENT_PRO = 'pl1')
   base17 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       Lab14 = WIDGET_label(base17,/Align_left, uname="Lab14", VALUE= "m/z IDs: ")
       ID2 = WIDGET_TEXT(base17, uname="ID2", VALUE='-1',  /EDITABLE,/ALL_EVENTS, YSIZE = 1,xSIZE = 4,uvalue=-9999)
       ID3 = WIDGET_TEXT(base17, uname="ID3", VALUE='-1',  /EDITABLE,/ALL_EVENTS, YSIZE = 1,xSIZE = 4,uvalue=-9999)
       ID4 = WIDGET_TEXT(base17, uname="ID4", VALUE='-1',  /EDITABLE,/ALL_EVENTS, YSIZE = 1,xSIZE = 4,uvalue=-9999)
       ID5 = WIDGET_TEXT(base17, uname="ID5", VALUE='-1',  /EDITABLE,/ALL_EVENTS, YSIZE = 1,xSIZE = 4,uvalue=-9999)
   
   But_4 = WIDGET_BUTTON(base, uname='But_4' ,value='update', event_pro='pl1')
   Text_SelFile = WIDGET_TEXT(base, uname="Text_SelFile", VALUE="selected File", /ALL_EVENTS,/wrap, /NO_NEWLINE, YSIZE = 3,uvalue=-9999)
   Lab2 = WIDGET_label(base,/Align_left, uname="Lab2", VALUE=" Specify Temp Steps: ")
   Lab3 = WIDGET_label(base,/Align_left, uname="Lab3", VALUE=" Default                                        Current ")
   base16 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       Dbk0 = WIDGET_TEXT(base16, uname="Doff", VALUE='12', /ALL_EVENTS, /EDITABLE,  YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Tbk0 = WIDGET_TEXT(base16, uname="Toff", VALUE='offset', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       Vbk0 = WIDGET_TEXT(base16, uname="Voff", VALUE='0', /ALL_EVENTS,/EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
  ; base3 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
   ;    Dbk0 = WIDGET_TEXT(base3, uname="Dbk0", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
    ;   Tbk0 = WIDGET_TEXT(base3, uname="Tbk0", VALUE='START background', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
     ;  Vbk0 = WIDGET_TEXT(base3, uname="Vbk0", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base4 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       Dbk1 = WIDGET_TEXT(base4, uname="Dbk1", VALUE='12', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Tbk1 = WIDGET_TEXT(base4, uname="Tbk1", VALUE='CYC bkgr ', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       Vbk1 = WIDGET_TEXT(base4, uname="Vbk1", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base5 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       D100 = WIDGET_TEXT(base5, uname="D100", VALUE='36', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       T100 = WIDGET_TEXT(base5, uname="T100", VALUE='CYC 100 C Step', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       V100 = WIDGET_TEXT(base5, uname="V100", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base6 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       D150 = WIDGET_TEXT(base6, uname="D150", VALUE='36', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       T150 = WIDGET_TEXT(base6, uname="T150", VALUE='CYC 150 C Step', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       V150 = WIDGET_TEXT(base6, uname="V150", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base7 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       D200 = WIDGET_TEXT(base7, uname="D200", VALUE='36', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       T200 = WIDGET_TEXT(base7, uname="T200", VALUE='CYC 200 C Step', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       V200 = WIDGET_TEXT(base7, uname="V200", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 10,uvalue=-9999)
   base8 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       D250 = WIDGET_TEXT(base8, uname="D250", VALUE='36', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       T250 = WIDGET_TEXT(base8, uname="T250", VALUE='CYC 250 C Step', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       V250 = WIDGET_TEXT(base8, uname="V250", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base9 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       D300 = WIDGET_TEXT(base9, uname="D300", VALUE='36', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       T300 = WIDGET_TEXT(base9, uname="T300", VALUE='CYC 300 C Step', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       V300 = WIDGET_TEXT(base9, uname="V300", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base10 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       D350 = WIDGET_TEXT(base10, uname="D350", VALUE='32', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       T350 = WIDGET_TEXT(base10, uname="T350", VALUE='CYC 350 C Step', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       V350 = WIDGET_TEXT(base10, uname="V350", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base11 = WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       Dspl = WIDGET_TEXT(base11, uname="Dspl", VALUE='-9999', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Tspl = WIDGET_TEXT(base11, uname="Tspl", VALUE='SPLIT  FILE', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       Vspl = WIDGET_TEXT(base11, uname="Vspl", VALUE='-9999', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base12= WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       Dmer = WIDGET_TEXT(base12, uname="Dmer", VALUE='-9999', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Tmer = WIDGET_TEXT(base12, uname="Tmer", VALUE='MERGE  FILE', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       Vmer = WIDGET_TEXT(base12, uname="Vmer", VALUE='-9999', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
  
   Lab5 = WIDGET_label(base,/Align_left, uname="Lab5", VALUE="Filter Information ")
   base13= WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       dID = WIDGET_TEXT(base13, uname="dID", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Lab6 = WIDGET_TEXT(base13, uname="Lab6", VALUE='Filter ID: ', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       ID = WIDGET_TEXT(base13, uname="vID", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base14= WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       dREP = WIDGET_TEXT(base14, uname="dREP", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Lab8 = WIDGET_TEXT(base14, uname="Lab8", VALUE='Replica ID: ', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       Rep = WIDGET_TEXT(base14, uname="vREP", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
    base15= WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       dFl= WIDGET_TEXT(base15, uname="dFl", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Lab7 = WIDGET_TEXT(base15, uname="Lab7", VALUE='Desorption Flow [ml/min]: ', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       Fl = WIDGET_TEXT(base15, uname="vFl", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
    base18= WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       dAFr= WIDGET_TEXT(base18, uname="dAFr", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Lab9 = WIDGET_TEXT(base18, uname="Lab9", VALUE='Aliquot [fract total Filt]  ', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       AFr = WIDGET_TEXT(base18, uname="vAFr", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base19= WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       dSV= WIDGET_TEXT(base19, uname="dSV", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Lab10 = WIDGET_TEXT(base19, uname="Lab10", VALUE='Samp Vol [m3]', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       SV = WIDGET_TEXT(base19, uname="vSV", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   base20= WIDGET_BASE(base,/row, XSIZE = 200, YSIZE = 20)
       dTR= WIDGET_TEXT(base20, uname="dTR", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 5,uvalue=-9999)
       Lab11 = WIDGET_TEXT(base20, uname="Lab11", VALUE='Treatment', /ALL_EVENTS, YSIZE = 1,xSIZE = 16,uvalue=-9999)
       TR = WIDGET_TEXT(base20, uname="vTR", VALUE='0', /ALL_EVENTS, /EDITABLE, YSIZE = 1,xSIZE = 7,uvalue=-9999)
   
 
   button = WIDGET_BUTTON(base, event_pro='read', uname='init',value='average data', uvalue=1)
   button = WIDGET_BUTTON(base, event_pro='playground', uname='init',value='custom evaluation', uvalue=1)
 WIDGET_CONTROL, base, /REALIZE
 XMANAGER, 'FiltPTR', base, /NO_BLOCK
end







 PRO FiltPTR_event, event
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_CONTEXT') THEN BEGIN
    contextBase = WIDGET_INFO(event.ID, FIND_BY_UNAME = 'base_3')
    WIDGET_DISPLAYCONTEXTMENU, event.ID, event.X, event.Y, contextBase
  ENDIF
END

 
PRO getDataDirFilt, event
  COMPILE_OPT hidden
  Path=''
  ; Prompt for path to catalog files 
 if( event.id eq WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_1')) then Path=dialog_pickfile( TITLE='Select directory BBB', /DIRECTORY ) else WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), gET_VALUE= path
  
  Path2=path+'filter\'
   print, "fileinfo"
  mmist=file_info(path2)
  if (mmist.directory eq 0) then begin 
          FILE_MKDIR, path2
          FILE_MKDIR, path2+'info'
          FILE_MKDIR, path2+'temp'
 endif
  if (path NE '') then begin
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), SET_VALUE= path
           Files=Filelst(path)
           
           makecsv, path2+'info\StartTimes.csv',transpose(Files.startTimes)
           WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Files'), SET_uVALUE= files.path
           WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Files'), SET_VALUE= string(max(size(files.path,/dimensions)),format='(I4)')+' Files in Directory'
           masslist=readcsv(strjoin([path,'unifiedmasslist.csv']))
           masslist=masslist[*,0]
            n_masses=max(size(masslist,/dimensions))
            WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Masses'), SET_VALUE= string(n_masses,format='(I4)')+' ions in masslist'
       
 
   
  endif
END
 


pro pl1,event
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_2'), get_VALUE=ind
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_12'), get_VALUE=m_ind
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'ID2'), get_VALUE=ID2
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'ID3'), get_VALUE=ID3
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'ID4'), get_VALUE=ID4
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'ID5'), get_VALUE=ID5
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Files'), get_uVALUE=files
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), gET_VALUE= path

    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T100'), get_uVALUE=av_VMR ;nmol/mol
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T250'), get_uVALUE=int_VMR ; s nmol/mol (multiply int_VMR by the oven Flow, in mol/s, to get the amount of substance in nmol)
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T150'), get_uVALUE=av_sig ;cps
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T200'), get_uVALUE=tot_cnts ;counts (sqrt of tot_cnts is the standard error)
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T300'), get_uVALUE=av_eng ;engineering data
 
 
 masslist=readcsv(strjoin([path,'unifiedmasslist.csv']))
masslist=masslist[*,0]
  n_masses=max(size(masslist,/dimensions))
  ;  masslist=[21.0206,38.0337,masslist[0:n_masses-2]]       
n_files=max(size(files,/dimensions))
n_masses=max(size(masslist,/dimensions))
ind=float(ind)
m_ind=float(m_ind)

if( event.id eq WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_2')) then if(abs(ind) lt n_files-1) then ind=ind-1
if( event.id eq WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_3')) then if(abs(ind) lt n_files-1) then ind=ind+1
if( event.id eq WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_12')) then if(m_ind gt -1) then m_ind=m_ind-1
if( event.id eq WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_13')) then if(m_ind lt n_masses-1) then m_ind=m_ind+1
if( event.id eq WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'But_4')) then update=1 else update=0

WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_2'), set_VALUE=string(ind,format='(I3)')
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_12'), set_VALUE=string(m_ind,format='(I3)')
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_SelFile'), set_VALUE=files[abs(ind)]


;********************************************************
;******  save parameters  split or merge files **********
;********************************************************

    name=strmid(files[abs(ind)],strlen(path),strlen(files[abs(ind)])-strlen(path))
    name2=strmid(name,strpos(name,'ppb')+3,strpos(name,'fdt')-strpos(name,'ppb')-3)
    parfile=path+'filter/info/par'+name2+'csv'
    helpi=file_info(parfile)
    if ( update eq 0) then begin
          if ( helpi.exists eq 1 and ind ge 0) then begin
                par=readcsv(parfile)
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Voff'), sET_VALUE= string(par[0],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vbk1'), sET_VALUE= string(par[1],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V100'), sET_VALUE= string(par[2],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V150'), sET_VALUE= string(par[3],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V200'), sET_VALUE= string(par[4],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V250'), sET_VALUE= string(par[5],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V300'), sET_VALUE= string(par[6],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V350'), sET_VALUE= string(par[7],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vspl'), sET_VALUE= string(par[8],format='(I6)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vmer'), sET_VALUE= string(par[9],format='(I6)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vID'),  sET_VALUE= string(par[10],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vREP'), sET_VALUE= string(par[11],format='(I4)')
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vFl'),  sET_VALUE= string(par[12])
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vAFr'), sET_VALUE= string(par[13])
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vSV'),  sET_VALUE= string(par[14])
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vTR'),  sET_VALUE= string(par[15])
                  
                
           endif else begin
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Doff'), gET_VALUE= p0
               ; WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dbk0'), gET_VALUE= p0
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dbk1'), gET_VALUE= p1
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'D100'), gET_VALUE= p2
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'D150'), gET_VALUE= p3
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'D200'), gET_VALUE= p4
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'D250'), gET_VALUE= p5
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'D300'), gET_VALUE= p6
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'D350'), gET_VALUE= p7
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dspl'), gET_VALUE= p8
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Dmer'), gET_VALUE= p9
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'dID'), gET_VALUE= p10
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'dREP'), gET_VALUE= p11
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'dFl'), gET_VALUE=  p12
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'dAFr'), gET_VALUE= p13
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'dSV'), gET_VALUE= p14
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'dTR'), gET_VALUE= p15
            ;    p0=strtrim(string(float(p0)+float(poff),format="(I4)"),2)
          ;      p1=strtrim(string(float(p1)+float(poff),format="(I4)"),2)
           ;     p2=strtrim(string(float(p2)+float(poff),format="(I4)"),2)
            ;    p3=strtrim(string(float(p3)+float(poff),format="(I4)"),2)
             ;   p4=strtrim(string(float(p4)+float(poff),format="(I4)"),2)
              ;  p5=strtrim(string(float(p5)+float(poff),format="(I4)"),2)
               ; p6=strtrim(string(float(p6)+float(poff),format="(I4)"),2)
                ;p7=strtrim(string(float(p7)+float(poff),format="(I4)"),2)
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Voff'), sET_VALUE= p0
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vbk1'), sET_VALUE= p1
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V100'), sET_VALUE= p2
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V150'), sET_VALUE= p3
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V200'), sET_VALUE= p4
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V250'), sET_VALUE= p5
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V300'), sET_VALUE= p6
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V350'), sET_VALUE= p7
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vspl'), sET_VALUE= p8
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vmer'), sET_VALUE= p9
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vID'), sET_VALUE= p10
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vREP'), sET_VALUE= p11
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vFl'), sET_VALUE=  p12
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vAFr'), sET_VALUE= p13
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vSV'), seT_VALUE= p14
                WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vTR'), sET_VALUE= p15
              endelse
    endif
 ;   if ( update eq 1) then begin
       ;   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Voff'), gET_VALUE= poff
        ;  if (abs(poff) gt 0) then begin
           ;     WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vbk0'), gET_VALUE= p0
        ;        WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vbk1'), gET_VALUE= p1
         ;       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V100'), gET_VALUE= p2
         ;       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V150'), gET_VALUE= p3
         ;       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V200'), gET_VALUE= p4
         ;       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V250'), gET_VALUE= p5
         ;       WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V300'), gET_VALUE= p6
         ;      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V350'), gET_VALUE= p7
            ;    p0=strtrim(string(float(p0)+float(poff),format="(I4)"),2)
            ;    p1=strtrim(string(float(p1)+float(poff),format="(I4)"),2)
            ;;    p2=strtrim(string(float(p2)+float(poff),format="(I4)"),2)
            ;    p3=strtrim(string(float(p3)+float(poff),format="(I4)"),2)
           ;     p4=strtrim(string(float(p4)+float(poff),format="(I4)"),2)
            ;    p5=strtrim(string(float(p5)+float(poff),format="(I4)"),2)
            ;    p6=strtrim(string(float(p6)+float(poff),format="(I4)"),2)
             ;   p7=strtrim(string(float(p7)+float(poff),format="(I4)"),2)
             ;   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vbk0'), sET_VALUE= p0
           ;     WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vbk1'), sET_VALUE= p1
            ;    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V100'), sET_VALUE= p2
            ;    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V150'), sET_VALUE= p3
            ;    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V200'), sET_VALUE= p4
            ;    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V250'), sET_VALUE= p5
            ;    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V300'), sET_VALUE= p6
            ;    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V350'), sET_VALUE= p7
        ;  endif
      ;    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Voff'), sET_VALUE= '0'
  ;  endif
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Voff'), gET_VALUE= p0
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vbk1'), gET_VALUE= p1
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V100'), gET_VALUE= p2
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V150'), gET_VALUE= p3
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V200'), gET_VALUE= p4
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V250'), gET_VALUE= p5
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V300'), gET_VALUE= p6
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'V350'), gET_VALUE= p7
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vspl'), gET_VALUE= p8
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vmer'), gET_VALUE= p9
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vID'), gET_VALUE= p10
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vREP'), gET_VALUE= p11
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vFl'), gET_VALUE=  p12
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vAFr'), gET_VALUE= p13
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vSV'), gET_VALUE= p14
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'vTR'), gET_VALUE= p15
       ;          par=float([p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15])
    if (p8 gt 3) then split=1 else split=0  
    if (p9 eq 0 and p8 lt 0) then merge=1 else if (p9 gt 3 and p8 lt 0) then merge=1 else merge=0  
    if (split eq 1) then begin
    
 
    
          ppb=readfloat(files[abs(ind)])
          dims=size(ppb,/dimensions)
          if( p8 lt dims[1]-5) then begin
                 cpsfile=strmid(files[abs(ind)],0,strpos(files[abs(ind)],'ppb')-4)+'cps/OCorr/corrcps'+strmid(files[abs(ind)],strpos(files[abs(ind)],'ppb')+3,20)+'UL.fdt'
                 cps=readfloat(cpsfile)
                engfile=strmid(files[abs(ind)],0,strpos(files[abs(ind)],'ppb')-7)+'EngData/EngData'+ strmid(files[abs(ind)],strpos(files[abs(ind)],'ppb')+3,20)+'.fdt'
                eng=readfloat(engfile)
                
                
                
                file2=files[abs(ind)]
                cpsfile2=cpsfile
                engfile2=engfile
                caldat, eng[0,0]+Julday(1,1,2009,0,0,0),Month1 , Day1 , Year1 , Hour1 , Minute1 , Second1
                caldat, eng[p8,0]+Julday(1,1,2009,0,0,0),Month2 , Day2 , Year2 , Hour2 , Minute2 , Second2
                pos=strtrim(string(Year1,format='(I4)'),2)+'.' 
                Year=strtrim(string(Year2,format='(I4)'),2)+'.' 
                Month=strtrim(string(Month2,format='(I4)'),2)+'.' & if(Month2 lt 10) then Month='0'+Month
                Day=strtrim(string(Day2,format='(I4)'),2)+'-' & if(Day2 lt 10) then Day='0'+Day
                Hour=strtrim(string(Hour2,format='(I4)'),2)+'h' & if(Hour2 lt 10) then Hour='0'+Hour
                Minute=strtrim(string(Minute2,format='(I4)'),2)+'m' & if(Minute2 lt 10) then Minute='0'+Minute
                Second=strtrim(string(Second2,format='(I4)'),2)+'s' & if(Second2 lt 10) then Second='0'+Second
                Newtime=Year+Month+Day+Hour+Minute+Second
                
                
                strput, file2,Newtime,strpos(file2,pos)
                strput, cpsfile2,Newtime,strpos(cpsfile2,pos)
                strput, engfile2,Newtime,strpos(engfile2,pos)
                ppb1=ppb[*,0:p8-1]
                ppb2=ppb[*,p8:*]
                cps1=cps[*,0:p8-1]
                cps2=cps[*,p8:*]
                eng1=eng[0:p8-1,*]
                eng2=eng[p8:*,*]
                makefloat, files[abs(ind)], ppb1
                makefloat, file2, ppb2
                makefloat, cpsfile, cps1
                makefloat, cpsfile2, cps2
                makefloat, engfile, eng1
                makefloat, engfile2, eng2
                getDataDirFilt, event
          endif
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vspl'), sET_VALUE=string(-9999,format='(I6)')
          p8=-9999
          print, 'split'         
    endif
    if (merge eq 1) then begin
          ppb=readfloat(files[abs(ind)])
          dims=size(ppb,/dimensions)
          if( p9 lt dims[1]-1) then begin
                ppb_next=readfloat(files[abs(ind)+1])
                 cpsfile=strmid(files[abs(ind)],0,strpos(files[abs(ind)],'ppb')-4)+'cps/OCorr/corrcps'+strmid(files[abs(ind)],strpos(files[abs(ind)],'ppb')+3,20)+'UL.fdt'
                 cps=readfloat(cpsfile)
                engfile=strmid(files[abs(ind)],0,strpos(files[abs(ind)],'ppb')-7)+'EngData/EngData'+ strmid(files[abs(ind)],strpos(files[abs(ind)],'ppb')+3,20)+'.fdt'
                eng=readfloat(engfile)
                
                cpsfile_next=strmid(files[abs(ind)+1],0,strpos(files[abs(ind)+1],'ppb')-4)+'cps/OCorr/corrcps'+strmid(files[abs(ind)+1],strpos(files[abs(ind)+1],'ppb')+3,20)+'UL.fdt'
                 cps_next=readfloat(cpsfile_next)
                engfile_next=strmid(files[abs(ind)+1],0,strpos(files[abs(ind)+1],'ppb')-7)+'EngData/EngData'+ strmid(files[abs(ind)+1],strpos(files[abs(ind)+1],'ppb')+3,20)+'.fdt'
                eng_next=readfloat(engfile_next)
                 
                
                   
                
                file2=files[abs(ind)]
                cpsfile2=cpsfile
                engfile2=engfile
                If (p9 ne 0) then begin
                        caldat, eng[0,0]+Julday(1,1,2009,0,0,0),Month1 , Day1 , Year1 , Hour1 , Minute1 , Second1
                        caldat, eng[p9,0]+Julday(1,1,2009,0,0,0),Month2 , Day2 , Year2 , Hour2 , Minute2 , Second2
                        pos=strtrim(string(Year1,format='(I4)'),2)+'.' 
                        Year=strtrim(string(Year2,format='(I4)'),2)+'.' 
                        Month=strtrim(string(Month2,format='(I4)'),2)+'.' & if(Month2 lt 10) then Month='0'+Month
                        Day=strtrim(string(Day2,format='(I4)'),2)+'-' & if(Day2 lt 10) then Day='0'+Day
                        Hour=strtrim(string(Hour2,format='(I4)'),2)+'h' & if(Hour2 lt 10) then Hour='0'+Hour
                        Minute=strtrim(string(Minute2,format='(I4)'),2)+'m' & if(Minute2 lt 10) then Minute='0'+Minute
                        Second=strtrim(string(Second2,format='(I4)'),2)+'s' & if(Second2 lt 10) then Second='0'+Second
                        Newtime=Year+Month+Day+Hour+Minute+Second
                        strput, file2,Newtime,strpos(file2,pos)
                        strput, cpsfile2,Newtime,strpos(cpsfile2,pos)
                        strput, engfile2,Newtime,strpos(engfile2,pos)
                endif
                ppb_next=[[ppb[*,p9:*]],[ppb_next]]
                ppb=ppb[*,0:p9-1]
                cps_next=[[cps[*,p9:*]],[cps_next]]
                cps=cps[*,0:p9-1]
                eng_next=[eng[p9:*,*],eng_next]
                eng=eng[0:p9-1,*]
                makefloat, files[abs(ind)], ppb
                makefloat, file2, ppb_next
                File_delete, files[abs(ind)+1]
                makefloat, cpsfile, cps
                makefloat, cpsfile2, cps_next
                File_delete, cpsfile_next
                makefloat, engfile, eng
                makefloat, engfile2, eng_next
                File_delete, engfile_next
                getDataDirFilt, event
          endif
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Vmer'), sET_VALUE=string(-9999,format='(I6)')
          p9=-9999
          print, 'merge'         
    endif
    name=strmid(files[abs(ind)],strlen(path),strlen(files[abs(ind)])-strlen(path))
    name2=strmid(name,strpos(name,'ppb')+3,strpos(name,'fdt')-strpos(name,'ppb')-3)
    parfile=path+'filter\info\par'+name2+'csv'
    makecsv, parfile,  transpose([p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15])

;****************
;****************
;****************



ppb=readfloat(files[abs(ind)])
 
if (M_ind eq -1) then yy=total(ppb[where(masslist gt 50),*],1) else yy=ppb[m_ind,*]
if(max(where(yy lt -9900)) gt 0) then yy[where(yy lt -9900)]=0
if (M_ind eq -1) then ytit='total VMR for m/z > 50 Da' else ytit='m/z '+strtrim(string(masslist[m_ind],format='(F8.3)'),2)+' [nmol/mol]'
  DEVICE, DECOMPOSED = 0 
  !P.MULTI = [0,1,1] 
  loadct,38
  plot, yy, /YNOZERO, XTITLE='Cycles', YTITLE=ytit , color=0, background=-1, charsize=2,thick=3
  loadct,31


if(ID2 gt -1) then oplot, ppb[ID2,*]*max(yy)/max(ppb[ID2,*]), thick=2, color=248
if(ID3 gt -1) then oplot, ppb[ID3,*]*max(yy)/max(ppb[ID3,*]), thick=2, color=249
if(ID4 gt -1) then oplot, ppb[ID4,*]*max(yy)/max(ppb[ID4,*]), thick=2, color=250
if(ID5 gt -1) then oplot, ppb[ID5,*]*max(yy)/max(ppb[ID5,*]), thick=2, color=251
  oplot, yy, thick=3, color=16
 p0=float(p0)
 p1=float(p1)
 p2=float(p2)
 p3=float(p3)
 p4=float(p4)
 p5=float(p5)
 p6=float(p6)
 p7=float(p7)
 
par=[p0,p0+p1,p0+p1+p2,p0+p1+p2+p3,p0+p1+p2+p3+p4,p0+p1+p2+p3+p4+p5,p0+p1+p2+p3+p4+p5+p6,p0+p1+p2+p3+p4+p5+p6+p7] 
bk1=quantile(yy[par[0]+indgen(par[1]-par[0])],0.04)
bk2=quantile(yy[par[0]+indgen(par[7]-par[0])],0.04)
if (bk2 lt bk1 and bk2 gt 0) then begin
lamda=36
N0=(bk1-bk2)*exp(max(p0)/lamda)
bk_fit=findgen(1000)/lamda
bk_fit=exp(-bk_fit)*N0+bk2
endif else bk_fit=fltarr(1000)
if (bk2 ge bk1 and bk2 gt 0) then bk_fit=fltarr(1000)+bk2

kleur=[2,96,128,3,10,12,140]

for k=0,6 do begin
oplot, [par[0+k],par[0+k]],[0,99999],color=kleur[k],thick=3
oplot, [par[1+k],par[1+k]],[0,99999],color=kleur[k],thick=3
sel=par[0+k]+lindgen(par[1+k]-par[0+k])
my=mean(yy[sel])
oplot, [par[0+k],par[1+k]], [my,my],color=0,thick=5
n=max(size(sel,/dimensions))
my=total(yy[sel])/n
oplot, [par[0+k],par[1+k]], [my,my],color=10,thick=5

if (tot_cnts[0] gt -1 and m_ind ge 0) then bg=bk_fit[par[0+k]+lindgen(par[1+k]-par[0+k])]
if (tot_cnts[0] gt -1 and m_ind ge 0) then oplot, [par[0+k],par[1+k]], [bg+av_VMR[abs(ind),m_ind,k],bg+av_VMR[abs(ind),m_ind,k]],color=kleur[k],thick=3
if (tot_cnts[0] gt -1 and m_ind ge 0) then oplot, [par[0+k],par[1+k]], [av_VMR[abs(ind),m_ind,k],av_VMR[abs(ind),m_ind,k]],color=kleur[k],thick=3
if (tot_cnts[0] gt -1 and m_ind ge 0) then err=av_VMR[abs(ind),m_ind,k]*sqrt(tot_cnts[abs(ind),m_ind,k])/tot_cnts[abs(ind),m_ind,k]
if (tot_cnts[0] gt -1 and m_ind ge 0) then oplot, [(par[0+k]+par[1+k])/2,(par[0+k]+par[1+k])/2], [bg+av_VMR[abs(ind),m_ind,k]-err,bg+av_VMR[abs(ind),m_ind,k]+err ],color=kleur[k],thick=3
endfor

oplot, lindgen(1000),bk_fit, color=12, thick=3

xyouts,par[0]+(par[1]-par[0])/4,max(yy),'bk', color=2, charsize=2, charthick=2
xyouts,par[1]+(par[2]-par[1])/4,max(yy),'100 C', color=96, charsize=2, charthick=2
xyouts,par[2]+(par[3]-par[2])/4,max(yy),'150 C', color=128, charsize=2, charthick=2
xyouts,par[3]+(par[4]-par[3])/4,max(yy),'200 C', color=3, charsize=2, charthick=2
xyouts,par[4]+(par[5]-par[4])/4,max(yy),'250 C', color=10, charsize=2, charthick=2
xyouts,par[5]+(par[6]-par[5])/4,max(yy),'300 C', color=12, charsize=2, charthick=2
xyouts,par[6]+(par[7]-par[6])/4,max(yy),'350 C', color=140, charsize=2, charthick=2




end


pro read,event
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_Files'), get_uVALUE=files
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_DataDir'), gET_VALUE= path
      
      
      
       masslist=files[0]
    helpi1=strmid(masslist,0,strpos(masslist,'ppb20')-4)
    helpi2=strmid(masslist,strpos(masslist,'ppb20')+3,strpos(masslist,'UL.fdt')-strpos(masslist,'ppb20')-3)
    masslist=helpi1+'IonList/MassIDs_'+helpi2+'UL.csv'  
    masslist=readcsv(masslist)
    
    
 ;    masslist=readcsv(strjoin([path,'unifiedmasslist.csv']))
  ;   masslist=masslist[*,0]
   ;   n_masses=max(size(masslist,/dimensions))
    ;masslist=[21.0206,38.0337,masslist[0:n_masses-2]]
     EngNames=readcsvstr(strjoin([path,'Export/EngData/','EngDataNames'+helpi2+'.csv']))
     n_files=max(size(files,/dimensions))
     n_masses=max(size(masslist,/dimensions))
     n_eng=max(size(EngNames,/dimensions))
  
    
    av_VMR=fltarr(n_files,n_masses,7)   ;nmol/mol
    int_VMR=fltarr(n_files,n_masses,7)    ; s nmol/mol (multiply int_VMR by the oven Flow, in mol/s, to get the amount of substance in nmol)
    av_sig=fltarr(n_files,n_masses,7)   ;cps
    tot_cnts=fltarr(n_files,n_masses,7)   ;counts (sqrt of tot_cnts is the standard error)
    av_eng=fltarr(n_files,n_eng,7)   ;engineering data
  
    filterInfo= fltarr(n_files,6)
    
    for ind=0 ,n_files-1 do begin
    
    ppb=readfloat(files[ind])
    cpsfile=files[ind]
     helpi1=strmid(cpsfile,0,strpos(cpsfile,'ppb20')-4)
    helpi2=strmid(cpsfile,strpos(cpsfile,'ppb20')+3,strpos(cpsfile,'UL.fdt')-strpos(cpsfile,'ppb20')-3)
    cpsfile=helpi1+'CPS/cps'+helpi2+'UL.fdt'  
    cps=readfloat(cpsfile)
    
    engfile=files[ind]
    helpi1=strmid(engfile,0,strpos(engfile,'ppb20')-7)
    helpi2=strmid(engfile,strpos(engfile,'ppb20')+3,strpos(engfile,'UL.fdt')-strpos(engfile,'ppb20')-3)
    engfile=helpi1+'engdata/engdata'+helpi2+'.fdt'  
    eng=readfloat(engfile)
    parfile=files[ind]
    helpi1=strmid(parfile,0,strpos(parfile,'ppb20')-14)
    helpi2=strmid(parfile,strpos(parfile,'ppb20')+3,strpos(parfile,'UL.fdt')-strpos(parfile,'ppb20')-3)
    parfile=helpi1+'filter/info/par'+helpi2+'UL.csv'  
    par=readcsv(parfile)
    par[7]=par[0]+par[1]+par[2]+par[3]+par[4]+par[5]+par[6]+par[7]
    par[6]=par[0]+par[1]+par[2]+par[3]+par[4]+par[5]+par[6]
    par[5]=par[0]+par[1]+par[2]+par[3]+par[4]+par[5]
    par[4]=par[0]+par[1]+par[2]+par[3]+par[4]
    par[3]=par[0]+par[1]+par[2]+par[3]
    par[2]=par[0]+par[1]+par[2]
    par[1]=par[0]+par[1]
    
  
    n_cycles=size(ppb,/dimensions) & n_cycles=max(n_cycles[1])
    time_cycle=24.0*3600.*(max(eng[*,0])-min(eng[*,0]))/(n_cycles-2)
   
    for ss=0,n_masses-2 do begin
        bk1=quantile(ppb[ss,par[0]+indgen(par[1]-par[0])],0.04)
        bk2=quantile(ppb[ss,par[0]+indgen(par[7]-par[0])],0.04)
        if (bk2 lt bk1 and bk2 gt 0) then begin
        lamda=180.0/time_cycle
        N0=(bk1-bk2)*exp(par[0]/lamda)
        bk_fit=exp(findgen(n_cycles)/lamda*(-1))*N0+bk2
        endif else bk_fit=fltarr(n_cycles)
        if (bk2 ge bk1 and bk2 gt 0) then bk_fit=fltarr(n_cycles)+bk2
   ;     if(bk2 gt 0 and bk2 ge bk1) then begin
    ;    print, [mean( ppb[ss,par[0]+indgen(par[7]-par[0])]), max(bk_fit),min(bk_fit), bk2]
     ;   print, 'haha'
      ;  endif
        ppb[ss,par[0]+indgen(par[7]-par[0])]=ppb[ss,par[0]+indgen(par[7]-par[0])]-bk_fit[par[0]+indgen(par[7]-par[0])]
       ; print,[masslist[ss], mean( ppb[ss,par[0]+indgen(par[7]-par[0])]), mean(bk_fit)]

  
    endfor
   
   
    for k=0,6 do begin
            sel=indgen(par[1+k]-par[0+k])+par[0+k] 
            n=float(max(size(sel,/dimensions)) )
            
            av_VMR[ind,*,k]=total(ppb[*,sel],2)/n 
   ;         print, [k,total(ppb[434,sel],2)/n,total(ppb[434,sel],2)/n- mean(ppb[434,sel]),n]
            int_VMR[ind,*,k]=av_VMR[ind,*,k]*n*time_cycle
            av_sig[ind,*,k]=total(cps[*,sel],2)/n
            tot_cnts[ind,*,k]=av_sig[ind,*,k]*n*time_cycle
            av_eng[ind,*,k]=total(eng[sel,*],1)/n 
    endfor        
       
     ; columns are: Filter ID, Replica, Desorption Flow, Aliquot Fraction, Sampled air volume, Treatment  
    for ooo=0,5 do filterInfo[ind,0+ooo]=par[10+ooo]

    print, strtrim(string(ind+1),2)+' of '+strtrim(string(n_files),2)+ ' '+files[ind]
    endfor                    
    ; allocate and save files
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T100'), set_uVALUE=av_VMR ;nmol/mol
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T250'), set_uVALUE=int_VMR ; s nmol/mol (multiply int_VMR by the oven Flow, in mol/s, to get the amount of substance in nmol)
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T150'), set_uVALUE=av_sig ;cps
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T200'), set_uVALUE=tot_cnts ;counts (sqrt of tot_cnts is the standard error)
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T300'), set_uVALUE=av_eng ;engineering data
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'T350'), set_uVALUE=FilterInfo ;FilterInfo
    makefloat, path+'/filter/av_VMR.ftd',av_VMR
    makefloat, path+'/filter/int_VMR.ftd',int_VMR
    makefloat, path+'/filter/av_sig.ftd',av_sig
    makefloat, path+'/filter/tot_cnts.ftd',tot_cnts
    makefloat, path+'/filter/av_eng.ftd',av_eng
         
    makecsv, path+'/filter/av_VMR_bk.csv',av_VMR[*,*,0]
    makecsv, path+'/filter/int_VMR_bk.csv',int_VMR[*,*,0]
    makecsv, path+'/filter/av_sig_bk.csv',av_sig[*,*,0]
    makecsv, path+'/filter/tot_cnts_bk.csv',tot_cnts[*,*,0]
    makecsv, path+'/filter/av_eng_bk.csv',av_eng[*,*,0]
  
    makecsv, path+'/filter/av_VMR_100.csv',av_VMR[*,*,1]
    makecsv, path+'/filter/int_VMR_100.csv',int_VMR[*,*,1]
    makecsv, path+'/filter/av_sig_100.csv',av_sig[*,*,1]
    makecsv, path+'/filter/tot_cnts_100.csv',tot_cnts[*,*,1]
    makecsv, path+'/filter/av_eng_100.csv',av_eng[*,*,1]
    
    makecsv, path+'/filter/av_VMR_150.csv',av_VMR[*,*,2]
    makecsv, path+'/filter/int_VMR_150.csv',int_VMR[*,*,2]
    makecsv, path+'/filter/av_sig_150.csv',av_sig[*,*,2]
    makecsv, path+'/filter/tot_cnts_150.csv',tot_cnts[*,*,2]
    makecsv, path+'/filter/av_eng_150.csv',av_eng[*,*,2]
     
     makecsv, path+'/filter/av_VMR_200.csv',av_VMR[*,*,3]
    makecsv, path+'/filter/int_VMR_200.csv',int_VMR[*,*,3]
    makecsv, path+'/filter/av_sig_200.csv',av_sig[*,*,3]
    makecsv, path+'/filter/tot_cnts_200.csv',tot_cnts[*,*,3]
    makecsv, path+'/filter/av_eng_200.csv',av_eng[*,*,3]
    makecsv, path+'/filter/av_VMR_250.csv',av_VMR[*,*,4]
    makecsv, path+'/filter/int_VMR_250.csv',int_VMR[*,*,4]
    makecsv, path+'/filter/av_sig_250.csv',av_sig[*,*,4]
    makecsv, path+'/filter/tot_cnts_250.csv',tot_cnts[*,*,4]
    makecsv, path+'/filter/av_eng_250.csv',av_eng[*,*,4]
   
    makecsv, path+'/filter/av_VMR_300.csv',av_VMR[*,*,5]
    makecsv, path+'/filter/int_VMR_300.csv',int_VMR[*,*,5]
    makecsv, path+'/filter/av_sig_300.csv',av_sig[*,*,5]
    makecsv, path+'/filter/tot_cnts_300.csv',tot_cnts[*,*,5]
    makecsv, path+'/filter/av_eng_300.csv',av_eng[*,*,5]
    makecsv, path+'/filter/av_VMR_350.csv',av_VMR[*,*,6]
    makecsv, path+'/filter/int_VMR_350.csv',int_VMR[*,*,6]
    makecsv, path+'/filter/av_sig_350.csv',av_sig[*,*,6]
    makecsv, path+'/filter/tot_cnts_350.csv',tot_cnts[*,*,6]
    makecsv, path+'/filter/av_eng_350.csv',av_eng[*,*,6]
   
   
    makecsv, path+'/filter/masslist.csv',transpose(masslist)
    makecsv, path+'/filter/filterInfo.csv',filterInfo
     
    
         
                        
print, 'haha'                        
end



PRO ____________key_routines
 end



function averaging,data, masslist, engdata, engnames, ind, carryover

 indexM=ind.m
 IS=[[ind.s1],[ind.s2],[ind.s3]]
 ISM=[ind.s1M,ind.s2M,ind.s3M]  
 FinalDataNames=['tSTRTav','tENDav','tSTRTsamp','tENDsamp','indM','indS1','indS2','indS3','k-counter','k','# avg',reform(engnames),string(masslist,format='(F9.3)')]
 FinalData=dblarr(1,max(size(FinalDataNames,/dimensions)))    
 FinalDataErr=dblarr(1,max(size(FinalDataNames,/dimensions)))    

 SAMPstart=fltarr(3)
 SAMPend=fltarr(3)
 SAMPvalue=fltarr(3)
 LastSvalue=fltarr(3)
 
 if (var_exists(carryover) gt -0.5) then begin
        SAMPstart=Carryover[0:2]
        SAMPend=Carryover[3:5]
        SAMPvalue=Carryover[6:8] 
        LastSvalue=Carryover[9:11]
 endif 

 timeSTARTsamp=SAMPstart[0]
 timeENDsamp=SAMPend[0]
  
 counter=0
 lleng=max(size(indexM,/dimensions))
 fil= max(where(abs(indexM[0:lleng-2]-indexM[1:lleng-1]) gt 50)) ;gives indices before change of indexM:  indexM[indices]-indexM[indices+1] >50
 if(fil eq -1)then fil=lleng
 time=double(engdata[*,0]) 
 avTime=double(ind.T)/(double(24)*double(3600)); in days   
 tstart=time[0]
 tend=time[lleng-1]
 step=(tend-tstart)/lleng
 

  
  Send10=fltarr(1000,3)
  Send20=fltarr(1000,3)
  Sstrt=fltarr(1000,3)

  for k=0,min([fil+2,lleng-2]) do begin ; for every cycle
       if (indexM[k] eq indexM[k+1] and ((time[k]-tstart)lt avTime)) then counter=counter+1 else begin
              if(counter ge 1) then begin
                      timeSTARTav=time[k-counter]
                      timeENDav=time[k]
                      indM=total(indexM[k-counter:k])/(counter+1)
                      indS=fltarr(3)
                      for gg=0,2 do begin
                              indxS=reform(IS[*,gg])
                              if( total(indxS[k-counter:k])/(counter+1) ne LastSvalue[gg]) then begin
                                      filti=where(indxS[k-counter:k]-[indxS[k-counter+1:k],indxS[k]] lt 0)
                                      if(indxS[k-counter] gt  LastSvalue[gg])then SAMPstart[gg]=time[k-counter] else if(max(filti) gt -0.5) then SAMPstart[gg]=time[max(filti+1)]
                                      filti=where(indxS[k-counter:k]-[indxS[k-counter+1:k],indxS[k]] gt 0)
                                      if(indxS[k-counter] lt  LastSvalue[gg])then begin 
                                              SAMPend[gg]=time[k-counter] 
                                              SAMPvalue[gg]=LastSvalue[gg]
                                      endif else if(max(filti) gt -0.5) then begin
                                              SAMPend[gg]=time[max(filti)]
                                              SAMPvalue[gg]=max(abs(indxS[k-counter:k]))
                                      endif
                                      LastSvalue[gg]=indxS[k]
                              endif
                     

                              if(indM-ISM[gg] ge 0 and indM-ISM[gg] lt 50 ) then begin
                                      timeSTARTsamp=SAMPstart[gg]
                                      timeENDsamp=SAMPend[gg]
                                      indS[gg]=SAMPvalue[gg]
                              endif
                      endfor
                      subset=[timeSTARTav,timeENDav,timeSTARTsamp,timeENDsamp,indM,indS,k-counter,k,counter+1,(total(engdata[k-counter:k,*],1))/(counter+1)]
                      FinalData=[FinalData,transpose([subset,total(data[*,k-counter:k],2)/(counter+1)])]
                      FinalDataErr=[FinalDataErr,transpose([subset,sqrt(total(data[*,k-counter:k],2))/total(data[*,k-counter:k],2)])]
                      
                      last=k
              endif
              counter=0
              tstart=time[k]
      endelse
 endfor
 carryover=[SAMPstart,SAMPend,SAMPvalue,LastSvalue]
 if(var_exists(last) eq -1) then last=-1 
 restind=last+1+indgen(lleng-last-1)
 restdata=data[*,restind]
 resteng=engdata[restind,*]
 mmss= d(finaldata)
 if (mmss[0] gt 1) then begin
        finaldata=finaldata[1:*,*]
        finaldataErr=finaldataErr[1:*,*]
 endif
 return, create_struct('names', finaldatanames, 'data', finaldata,'dataErr', finaldataErr,'restdata',restdata,'resteng',resteng,'carryover',carryover)
 
 
end




function Cal3pt, peaklist,a,t0,ex,sampint,lib, mode,instrument

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; 3-point re calibration
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
 
      Flib=-1
      if( mode eq 0) then begin
              Flib=where(2*floor(floor(lib+0.4)/2) ne floor(lib+0.4) or lib lt 50);selects odd masses
              if(max(Flib) gt 0) then lib=lib[Flib] 
      endif else begin
              Flib=where(2*floor(floor(lib+0.4)/2) eq floor(lib+0.4) or lib lt 50);selects odd masses
              if(max(Flib) gt 0) then lib=lib[Flib] 
      endelse

      
   
   
    massesFINE=m2t(peaklist[*,7], a,t0,ex,SampInt)
 a3=a
 t03=t0
 ex3=ex
 
   M1a=max(getpar('M1a'))
  M1b=max( getpar('M1b'))
  M2a= max(getpar('M2a'))
   M2b=max(getpar('M2b'))
  M3a= max(getpar('M3a'))
  M3b= max(getpar('M3b'))
  Mset=[M1a,M2a,M3a]   
   tolli=max(getpar('tol_ppm'))
 

   scores=0
   
   Mcal=fltarr(3,8)
   Mcal[0,*]=[M1a,M1a,M1a,M1a,M1b,M1b,M1b,M1b]
   Mcal[1,*]=[M2a,M2a,M2b,M2b,M2a,M2a,M2b,M2b]
   Mcal[2,*]=[M3a,M3b,M3a,M3b,M3a,M3b,M3a,M3b]
   
   for r=0,7 do begin
   M1=Mcal[0,r]
  M2=Mcal[1,r]
  M3=Mcal[2,r]
   
    I1=where(abs(massesFINE-M1) lt tolli*M1*1e-6) & I1=(I1[0])
    I2=where(abs(massesFINE-M2) lt tolli*M2*1e-6) & I2=(I2[0])
    I3=where(abs(massesFINE-M3) lt tolli*M3*1e-6) & I3=(I3[0])
   
   If( I1 gt -0.5 and I2 gt -0.5 and I3 gt -0.5) then begin
          T1=double(peaklist[I1,7])*SampInt/1e-10
          T2=double(peaklist[I2,7])*SampInt/1e-10
          T3=double(peaklist[I3,7])*SampInt/1e-10
          i=-9000
          ex4=double(0.5+float(i)/30000000)
          Const=double((T3-T1)/(T2-T1))
          diff=abs(((M3^ex4-M1^ex4) /(M2^ex4-M1^ex4))-Const)
          for i=-8999,9000 do begin
                  exi=double(0.5+double(i)/30000000)
                  diffi=abs(((M3^exi-M1^exi) /(M2^exi-M1^exi))-Const)
                  ; print, [exi, diffi]
                  if(diffi lt diff) then begin
                          diff=diffi
                          ex4=double(exi)
                  endif
          endfor
          a4=double((T2-T1)/(M2^ex4-M1^ex4))
          t04=double(T1-a4*M1^ex4)
  
  
  
  
        if(ex4 gt max(getpar('exMin'))and ex4 lt max(getpar('exMax')))then tst=testSCALE(m2t(peaklist[*,7],a4,t04,ex4, SampInt), lib,instrument) else tst=create_struct('scorppm',-1)
     
     print, [M1,M2,M3,tst.scorppm]
     
     if(tst.scorppm gt scores) then begin
     scores=tst.scorppm
     ex3=ex4
     t03=t04
     a3=a4
     Mset=[M1,M2,M3]
     
     endif
         
    endif
    endfor
    
    
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; variation of 3-point calibration
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
     varset_ppm=[20,9,3]
     if (var_exists(mset) gt -0.5) then for s=0,2 do begin
             var_ppm=varset_ppm[s]
             M1=Mset[0]
             d1=M1*var_ppm*1e-6
             M1var=[M1-1.5*d1,M1-0.5*d1,M1+0.5*d1,M1+1.5*d1]
             M1var=[M1var,M1var,M1var,M1var,     M1var,M1var,M1var,M1var,     M1var,M1var,M1var,M1var,     M1var,M1var,M1var,M1var]
            
             M2=Mset[1]
             d2=M2*var_ppm*1e-6
             M2var=[M2-1.5*d2,M2-1.5*d2,M2-1.5*d2,M2-1.5*d2,     M2-0.5*d2,M2-0.5*d2,M2-0.5*d2,M2-0.5*d2,   M2+0.5*d2,M2+0.5*d2,M2+0.5*d2,M2+0.5*d2,   M2+1.5*d2,M2+1.5*d2,M2+1.5*d2,M2+1.5*d2]
             M2var=[M2var,M2var,M2var,M2var]
           
             M3=Mset[2]
             d3=M3*var_ppm*1e-6
             M3var=fltarr(16)+M3
             M3var=[M3var-1.5*d3,M3var-0.5*d3,M3var+0.5*d3,M3var+1.5*d3]
           
              
             for r=0,63 do begin
                  M1=M1var[r]
                  M2=M2var[r]
                  M3=M3var[r]
                  I1=where(abs(massesFINE-M1) lt tolli*M1*1e-6) & I1=(I1[0])
                  I2=where(abs(massesFINE-M2) lt tolli*M2*1e-6) & I2=(I2[0])
                  I3=where(abs(massesFINE-M3) lt tolli*M3*1e-6) & I3=(I3[0])
                  If( I1 gt -0.5 and I2 gt -0.5 and I3 gt -0.5) then begin
                          T1=double(peaklist[I1,7])*SampInt/1e-10
                          T2=double(peaklist[I2,7])*SampInt/1e-10
                          T3=double(peaklist[I3,7])*SampInt/1e-10
                          i=-9000
                          ex4=double(0.5+float(i)/30000000)
                          Const=double((T3-T1)/(T2-T1))
                          diff=abs(((M3^ex4-M1^ex4) /(M2^ex4-M1^ex4))-Const)
                          for i=-8999,9000 do begin
                                  exi=double(0.5+double(i)/30000000)
                                  diffi=abs(((M3^exi-M1^exi) /(M2^exi-M1^exi))-Const)
                                  if(diffi lt diff) then begin
                                          diff=diffi
                                          ex4=double(exi)
                                  endif
                          endfor
                          a4=double((T2-T1)/(M2^ex4-M1^ex4))
                          t04=double(T1-a4*M1^ex4)
                          if( min(abs(m2t(peaklist[*,7],a4,t04,ex4, SampInt)-21.0221)) lt 0.002) then begin ; allow maximum deviations of 2 mDa
                          if(ex4 gt max(getpar('exMin'))and ex4 lt max(getpar('exMax')))then begin
                           tst=testSCALE(m2t(peaklist[*,7],a4,t04,ex4, SampInt), lib, instrument)
                          print, [0,0,M1,M2,M3,tst.scorppm]
                          if(tst.scorppm gt scores) then begin
                                  scores=tst.scorppm
                                  ex3=ex4
                                  t03=t04
                                  a3=a4
                                  Mset=[M1,M2,M3]
                          endif
                          endif
                          endif
                  endif
             endfor
     endfor
    
    return, create_struct('a',a3,'t0',t03,'ex',ex3,'Mset',Mset)
end




function calcppb, cpsdata, masslist, Prim1data, Prim2data, pdrift, udrift,tdrift
; calculates volume mixing ratio  

ppbdata=cpsdata
Prim1=reform(Prim1data) 
Prim2=reform(Prim2data)
pdrift=reform(pdrift)
udrift=reform(udrift)
tdrift=reform(tdrift)


k19=max(getpar('k19')) ;units e-9
k37=max(getpar('k37')) ;units e-9

transe=corrtr(findgen(2000))



nDim=SIZE(cpsdata, /n_DIMENSIONS)
AnzMass=max(size([masslist],/dimensions))

if (nDim eq 1 and AnzMass eq 1) then  begin
        ppbdata=reform(ppbdata)
        ppbdata=transpose([[ppbdata],[ppbdata]])
endif 

if (nDim eq 1 and AnzMass gt 1) then cyc=1 else if (max(SIZE(ppbdata, /n_DIMENSIONS)) eq 1) then cyc=1 else begin
        Dim=SIZE(ppbdata, /DIMENSIONS)
        Cyc=Dim[1]
endelse

d=max(getpar('reactionlength'))  ;reactionlength in cm
 mu0=max(getpar('reduced_mobility')) ;reduced mobility of H3O+ in N2

 
mu=mu0*(1013.25/pdrift)*(tdrift/273.15) ;[=] cm2/Vs
trxn=d/(((mu * udrift)/d))  ; reaction time in s  
Nmolec=24.63*298*pdrift/(1013.25 *tdrift)


for i=0,AnzMass-1 do begin
        aa=ppbdata[i,*]/transe[floor(masslist[i])]
        bb=Prim1*k19/transe[21]+ Prim2*k37/transe[38]
        cc=trxn*Nmolec
        ppbdata[i,*]=aa/(bb*cc)
endfor

if(nDim eq 1) then ppbdata=ppbdata[0,*]

Return, ppbdata
end

function calcppbold, cpsdata, masslist, Prim1data, Prim2data, pdrift, udrift,tdrift
; calculates volume mixing ratio  

ppbdata=cpsdata
Prim1=reform(Prim1data) 
Prim2=reform(Prim2data)
pdrift=reform(pdrift)
udrift=reform(udrift)
tdrift=reform(tdrift)


k19=max(getpar('k19')) ;units e-9
k37=max(getpar('k37')) ;units e-9





nDim=SIZE(cpsdata, /n_DIMENSIONS)
AnzMass=max(size([masslist],/dimensions))

if (nDim eq 1 and AnzMass eq 1) then  begin
        ppbdata=reform(ppbdata)
        ppbdata=transpose([[ppbdata],[ppbdata]])
endif 

if (nDim eq 1 and AnzMass gt 1) then cyc=1 else if (max(SIZE(ppbdata, /n_DIMENSIONS)) eq 1) then cyc=1 else begin
        Dim=SIZE(ppbdata, /DIMENSIONS)
        Cyc=Dim[1]
endelse

d=max(getpar('reactionlength'))  ;reactionlength in cm
 mu0=max(getpar('reduced_mobility')) ;reduced mobility of H3O+ in N2

 
mu=mu0*(1013.25/pdrift)*(tdrift/273.15) ;[=] cm2/Vs
trxn=d/(((mu * udrift)/d))  ; reaction time in s  
Nmolec=24.63*298*pdrift/(1013.25 *tdrift)


for i=0,AnzMass-1 do begin
ppbdata[i,*]=((ppbdata[i,*]/corrtr(masslist[i]))/(Prim1*k19/corrtr(21)+ Prim2*k37/corrtr(38)))/(trxn*Nmolec)  
endfor

if(nDim eq 1) then ppbdata=ppbdata[0,*]


Return, ppbdata
end


function CalCrude, peaklist,sampint,lib,instrument
;;;;;;;;;;;;;;;;;;;;;;;;;;
   ; Crude/Rough Mass scale calibration  
   ;;;;;;;;;;;;;;;;;;;;;;;;;;  
    a=-9999
    t0=-9999
    meanDev=1000
    nnn=max(getpar('PeaksToConsider'))
    dims=size(peaklist,/dimensions)  
    Top16=peaklist[sort(peaklist[*,6]),*] 
    Top16all=Top16[dims[0]-nnn:dims[0]-1,*]
    Top16=Top16all[*,7]
    crudePairs=[[19.018, 37.028], [29.997, 45.992], [19.018, 29.997], [31.989, 45.992], [19.018, 31.989]]
    calPairs=  [[21.022, 59.049], [30.994, 47.997], [21.022, 30.994], [33.994, 47.997], [21.022, 33.994]]
    ; NO  29.997   30.994
    ; NO2  45.992  47.9966
    ; O2  31.989 33.9935

         print, 'MassLow, timeLow, MassHigh, timeHigh, mean deviation, a, t0, adopted'
  ii=0                         
   for i=0,nnn-1 do begin
          timeLow=max(float(Top16[i]))
          for j=0,nnn-1 do begin
                  timeHigh=max(float(Top16[j]))
                  if (timeHigh gt timeLow) then for k=0,4 do begin
                          massLow=crudePairs[0,k]
                          massHigh=crudePairs[1,k]
                          massLowCal=calPairs[0,k]
                          massHighCal=calPairs[1,k]
                          tt0 = (sqrt(massLow)*timeHigh*(SampInt/1e-10)-sqrt(massHigh)*timeLow*(SampInt/1e-10))/(sqrt(massLow)-sqrt(massHigh))
                          aa = (timeLow*(SampInt/1e-10)-tt0)/sqrt(massLow)
                          masses=m2t(peaklist[*,7],aa,tt0,0.5,SampInt,/time)
                          if (min(abs(masses-massLowCal)) lt 0.02 and min(abs(masses-massHighCal)) lt 0.1) then begin 
                                 ja=0
                                 limLow=min(abs(masses-massLowCal))
                                 limHigh=min(abs(masses-massHighCal))
                                 i1=sort(abs(masses-massLowCal)) 
                                 timeLowCal=peaklist[i1[0],7]
                                 i2=sort(abs(masses-massHighCal)) 
                                 timeHighCal=peaklist[i2[0],7]
                                 tt0 = max((sqrt(massLowCal)*timeHighCal*(SampInt/1e-10)-sqrt(massHighCal)*timeLowCal*(SampInt/1e-10))/(sqrt(massLowCal)-sqrt(massHighCal)))
                                 aa = max((timeLowCal*(SampInt/1e-10)-tt0)/sqrt(massLowCal))
                                 masses=m2t(peaklist[*,7],aa,tt0,0.5,SampInt,/time)
                                 test=abs(testSCALE3(masses,lib, instrument))
                                  if (test lt meanDev) then begin
                             ;    if (test lt meanDev and tt0 gt getpar('t0Min') and tt0 lt getpar('t0Max')and aa gt getpar('aMin') and aa lt getpar('aMax')) then begin
                                        meanDev=test
                                        a=aa
                                        t0=tt0
                                        ja=2
                                       print, [ii,min(abs(masses-massLowCal)),min(abs(masses-massHighCal)) ,massLowCal, timeLow, massHighCal, timeHigh,test,limLow,limHigh, aa,tt0, ja]
                      
                                 endif
                                ii=ii+1
                            endif
                  
                  
                  endfor
          
          
          endfor
   
   
   endfor
   
   
        print, '____________________________'   
            print, 'mean deviation, a, t0'
            print, [meanDev,a,t0]
            print, '____________________________'              
   
   

return, create_struct('a',a,'t0',t0,'ex',0.5,'Top16',Top16all)
end



function CalFine, peaktimes,lib, a,t0, SampInt, mode,instrument
analytics=0

ex=0.5
;print, 'CalFine results:    scores',max(sco), '  a:',a,'   t0:',t0

;for r=0,4 do begin
for r=0,3 do begin
;for r=0,2 do begin
        a_raw=a
        t0_raw=t0
        ex_raw=ex
        for q=-3,3 do begin
                ;mag=[1,3,9,27,80]
                mag=[1,3.5,12,43]
                ;mag=[1,7,49]
                ex1=ex_raw+0.00001*q/mag[r]
                grid=29
                stepa=50.002/(grid*mag[r])
                stept0=500.01/(grid*mag[r])
                if(grid eq 2*floor(grid/2)) then grid=grid+1
                sco=findgen(grid,grid)/1e8
                sco120=findgen(grid,grid)/1e8
                for k=-floor(grid/2),floor(grid/2) do if(ex1 gt max(getpar('exMin'))and ex1 lt max(getpar('exMax')))then begin
                        aa=a_raw+stepa*k
                        if(analytics  gt 0.5) then print, '      k=',k+floor(grid/2), '     a=', aa,'    t0=',t0-floor(grid/2)*stept0,t0+floor(grid/2)*stept0
                        for l=-floor(grid/2),floor(grid/2) do begin
                                t00=t0_raw+stept0*l
                                tst=testSCALE2(m2t(peaktimes,aa,t00,ex1,SampInt,/time), lib,instrument)
                                sco[k+floor(grid/2),l+floor(grid/2)]=tst.scor
                                sco120[k+floor(grid/2),l+floor(grid/2)]=tst.scor120
                        endfor
                        if(analytics  gt 0.5) then  PRINT, FORMAT='(I,I,I,I,I,I,I,I,I,I,I,I,I,I)',floor(sco[k+floor(grid/2),*])
                endif   
                scomist=sco
                maxi=where(scomist eq max(scomist))
                maxi=maxi[0]
                MAXX120=sco120[where(sco120 eq max(sco120))]
                MAXX120=MAXX120[0]
                while(sco120[maxi] lt 0.9*MAXX120) do begin
                        scomist[maxi]=0
                        maxi=where(scomist eq max(scomist))
                        maxi=maxi[0]
                endwhile
                l=floor(maxi/grid)
                k=maxi-grid*l
                a1=max(a_raw+stepa*(k-floor(grid/2)))
                t01=max(t0_raw+stept0*(l-floor(grid/2)))
                old=testSCALE2(m2t(peaktimes,a,t0,ex,SampInt,/time),lib,instrument)
                new=testSCALE2(m2t(peaktimes,a1,t01,ex1,SampInt,/time), lib,instrument)
                ;  print,q, a, t0, ex, old.scor
                print, q, a1, t01, ex1, new.scor
                if(new.scor gt old.scor and new.scor120 gt old.scor120*0.9) then begin
                        a=a1
                        t0=t01
                        ex=ex1
                endif
                
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
                
                if(analytics  gt 0.5) then  begin
                        sco[floor(grid/2),floor(grid/2)]=0
                        print, 'max',sco[maxi],'k',k,'l',l
                        scoS=BYTSCL(sco)
                        mist=fltarr(grid*12,grid)
                        for i=0,grid-1 do for j=0,11 do mist[i*12+j,*]=scoS[i,*]
                        image=fltarr(grid*12,grid*12)
                        for i=0,grid-1 do for j=0,11 do image[*,i*12+j]=mist[*,i]
                        length=max(size(image,/dimensions))
                        legend=fltarr(length,50)
                        for iii=0,49 do legend[*,iii]=max(image)*findgen(length)/length
                        image=[image,transpose(legend)]
                        bottom=fltarr(length+50,50)
                        image=[[bottom],[image]]
                        for k=-floor(grid/2),floor(grid/2) do begin
                                aa=a_raw+stepa*k
                                t00=t0_raw+stept0*k
                                i=k+floor(grid/2)
                                sieb=rotate(drucksieb(string(aa, FORMAT='(F7.2)'),1),1)
                                dimsieb=size(sieb,/dimensions)
                                dat=image[12*i+2:12*i+2+dimsieb[0]-1,5:5+dimsieb[1]-1]
                                dat[where(sieb eq -9999)]=max(image)
                                image[12*i+2:12*i+2+dimsieb[0]-1,5:5+dimsieb[1]-1]=dat
                                sieb=drucksieb(string(t00, FORMAT='(F6.2)'),1)
                                dimsieb=size(sieb,/dimensions)
                                dat=image[length-50:length-50+dimsieb[0]-1,52+12*i:52+12*i+dimsieb[1]-1]
                                dat[where(sieb eq -9999)]=max(image)
                                image[length-50:length-50+dimsieb[0]-1,52+12*i:52+12*i+dimsieb[1]-1]=dat
                        endfor
                        sieb=drucksieb(string(a1, FORMAT='(F8.2)'),1)
                        dimsieb=size(sieb,/dimensions)
                        dat=image[5:5+dimsieb[0]-1,76:76+dimsieb[1]-1]
                        dat[where(sieb eq -9999)]=max(image)
                        image[5:5+dimsieb[0]-1,76:76+dimsieb[1]-1]=dat
                        sieb=drucksieb(string(t01, FORMAT='(F7.2)'),1)
                        dimsieb=size(sieb,/dimensions)
                        dat=image[5:5+dimsieb[0]-1,64:64+dimsieb[1]-1]
                        dat[where(sieb eq -9999)]=max(image)
                        image[5:5+dimsieb[0]-1,64:64+dimsieb[1]-1]=dat
                        sieb=drucksieb(string(ex1, FORMAT='(F8.6)'),1)
                        dimsieb=size(sieb,/dimensions)
                        dat=image[5:5+dimsieb[0]-1,52:52+dimsieb[1]-1]
                        dat[where(sieb eq -9999)]=max(image)
                        image[5:5+dimsieb[0]-1,52:52+dimsieb[1]-1]=dat
                        sieb=drucksieb(string(max(sco), FORMAT='(F7.3)'),1)
                        dimsieb=size(sieb,/dimensions)
                        dat=image[5:5+dimsieb[0]-1,94:94+dimsieb[1]-1]
                        dat[where(sieb eq -9999)]=max(image)
                        image[5:5+dimsieb[0]-1,94:94+dimsieb[1]-1]=dat
                        DEVICE, DECOMPOSED = 0 
                        window,26+q-5*r, xsize = length+50, ysize=length+50
                        loadct,20
                        tvscl, image
                        window,0
                endif
        endfor
endfor



s1=create_struct('a',a,'t0',t0,'ex',ex)
return, s1
end




function CorrPoissDead, TofData,extractions, sampint
dim=size(TofData,/dimensions)
bins=dim[0]
cyc=dim[1]
nedt=floor(getpar('NonExtendingDeadTime')*10*1e-10/sampint)  ;i.e 15 ns, non-extending dead time
edt=floor(getpar('ExtendingDeadTime')*10*1e-10/sampint) ;i.e 1.3, extending dead time
for rr=1,cyc-1 do begin ;loop over cycles
        TofDataCor=TofData[*,rr]
        indx=where(1000*1e-10*TofData[*,rr]/(extractions*sampint) gt 1) ; bei 0.2ns jedes 500ste mal ein count; bei 0.1ns jedes 1000ste mal
        n=max(size(indx,/dimensions))
        for ss=0,n-1 do begin
                Si=TofData[indx[ss],rr]
                r=extractions
                Sj=total(TofData[indx[ss]-nedt+lindgen(nedt-edt),rr])/r
                nj=total(TofDataCor[indx[ss]-edt+lindgen(edt)])/r
                ni=-r*alog(1-(exp(nj)*Si/(r*1-Sj)))
       
       
               
                
                TofDataCor[indx[ss]]=ni
         endfor
        TofData[*,rr]=TofDataCor
 endfor
;print, systime(1)-mistt
return, TofData
end


function corrtr, mass
;the mass transmission of the TOF from m0-m1500...
tr=max(getpar('P0'))+max(getpar('P1'))*mass+max(getpar('P2'))*mass^2+max(getpar('P3'))*mass^3+max(getpar('P4'))*mass^4+max(getpar('P5'))*mass^5+max(getpar('P6'))*mass^6
return,tr
end


function DetectPeaks, SumSpectrum, a,t0,ex,SampInt,clean=clean
  compile_opt idl2
  if (keyword_set(clean)) then cln=1 else cln=0
      faktor=floor(1e-10/sampint)
     if(faktor eq 0) then faktor=1
  savgolFilter = SAVGOL(9*faktor, 9*faktor, 0, floor(10/faktor))
        dataX0=CONVOL(SumSpectrum, savgolFilter, /EDGE_TRUNCATE)   
  derdataX0=CONVOL(deriv(dataX0), savgolFilter, /EDGE_TRUNCATE)   
    savgolFilter = SAVGOL(9*faktor, 9*faktor, 0, floor(6/faktor))
        dataX1=CONVOL(SumSpectrum, savgolFilter, /EDGE_TRUNCATE)
  derdataX1=CONVOL(deriv(dataX1), savgolFilter, /EDGE_TRUNCATE)   
    savgolFilter = SAVGOL(14*faktor, 14*faktor, 0,  floor(6/faktor))
        dataX2=CONVOL(SumSpectrum, savgolFilter, /EDGE_TRUNCATE)
  derdataX2=CONVOL(deriv(dataX2), savgolFilter, /EDGE_TRUNCATE)   
   savgolFilter = SAVGOL(20*faktor, 20*faktor, 0,  floor(6/faktor))
   dataX3=CONVOL(SumSpectrum, savgolFilter, /EDGE_TRUNCATE)
  derdataX3=CONVOL(deriv(dataX3), savgolFilter, /EDGE_TRUNCATE)   
  Npts=size(SumSpectrum,/dimensions) 
  Thresh1=m2t(64.6,a,t0,ex,SampInt)
  Thresh2=m2t(144.6,a,t0,ex,SampInt)
  Thresh3=m2t(324.6,a,t0,ex,SampInt)
  data2=SumSpectrum
  if(Npts gt Thresh1) then data2[0:Thresh1-1]=dataX0[0:Thresh1-1] else data2=dataX0
  if(Npts gt Thresh2) then data2[Thresh1:Thresh2-1]=dataX1[Thresh1:Thresh2-1] else  if(Npts gt Thresh1) then data2[Thresh1:*]=dataX1[Thresh1:*]
  if(Npts gt Thresh3) then begin
          data2[Thresh2:Thresh3-1]=dataX2[Thresh2:Thresh3-1] 
          data2[Thresh3:Npts-1]=dataX3[Thresh3:Npts-1]
  endif else  if(Npts gt Thresh2) then data2[Thresh2:Npts-1]=dataX2[Thresh2:Npts-1]
  derivData=deriv(Data2)
  
  if(Npts gt Thresh1) then derivData[0:Thresh1-1]=derdataX0[0:Thresh1-1] else data2=derdataX0
  if(Npts gt Thresh2) then derivData[Thresh1:Thresh2-1]=derdataX1[Thresh1:Thresh2-1] else if(Npts gt Thresh1) then derivData[Thresh1:*]=derdataX1[Thresh1:*]
  if(Npts gt Thresh3) then begin
          derivData[Thresh2:Thresh3-1]=derdataX2[Thresh2:Thresh3-1] 
          derivData[Thresh3:Npts-1]=derdataX3[Thresh3:Npts-1]
  endif else  if(Npts gt Thresh2) then derivData[Thresh2:Npts-1]=derdataX2[Thresh2:Npts-1]
  x=lindgen(max(Npts)-1)
  start=max([floor(m2t(getpar('Min_Mass'),a,t0,ex,SampInt)),4000])
  ende= max(Npts)-1
  peaklist=fltarr(20000,9)
  PeakCount=0
  i=1L+start+200
  RunNum=0
  while (i LT ende-1000) do begin
          munit=floor(i-m2t(m2t(i,a,t0,ex,SampInt,/time)-1,a,t0,ex,SampInt)) ;TOF bins per mass unit
          step=floor(munit/15)
          per1=derivData[(i-munit):i]
          f1=SumSpectrum[(i-munit):i]
          per2=derivData[i:(i+munit)]
          f2=SumSpectrum[i:(i+munit)]
          if(max(where(f1 ne 0)) gt -0.5) then threshA=median(abs(per1[where(f1 ne 0)])) else threshA=0
          if(max(where(f2 ne 0)) gt -0.5) then threshB=median(abs(per2[where(f2 ne 0)])) else threshB=0
          thresh=min([threshA,threshB])*6
       ;   thresh=min([median(abs(derivData[(i-munit):i])),median(abs(derivData[i:(i+munit)]))])*6; 6 times the median slope (of smoothed spectrum) in the neighborhood
          i=i+step
          condi=0
          if(max(derivData[i:i+step-1]) GT thresh)then condi=1 
          if(condi GT 0.5) then begin
                  RunNum=RunNum+1
                  di=0
                  while (derivData[i+di] LT thresh) do begin
                          IF (di eq step) THEN BREAK 
                          di=di+1 
                  endwhile
                  i=i+di
                  ipeakstart=i ;peak emerges above noise (thresh or thresh2)
                  islopemax=i      ; position of steepest slope (max of deriv)
                  ipeakmax=0L     ; peak maximum (deriv from pos to neg)
                  threshneg=0
                  islopemin=0
                  ipeakend=0
                  j=0
                  while(ipeakend eq 0) do begin
                          j=j+1
                          if(ipeakmax EQ 0) then if(derivData[i+j] GT derivData[islopemax]) then islopemax=i+j
                          if(ipeakmax EQ 0) then if(derivData[i+j] LT 0) then ipeakmax=i+j-1
                          if(ipeakmax GT 1) then if(islopemin EQ 0) then if(derivData[i+j] LT 0) then  islopemin=i+j
                          if(islopemin GT 1) then  if(derivData[i+j] LT derivData[islopemin]) then  islopemin=i+j
                          if(islopemin GT 1) then if(derivData[i+j] GT 0) then ipeakend=i+j
                  endwhile 
                  peakbroadness=m2t(ipeakmax,a,t0,ex,SampInt,/time)/(m2t(ipeakend,a,t0,ex,SampInt,/time)-m2t(ipeakstart,a,t0,ex,SampInt,/time))
                  slopemax=derivData[islopemax]
                  slopemin=derivData[islopemin]
                  countsmax=Data2[ipeakmax]
                  countsmin=min(Data2[ipeakstart:ipeakend])
                  if (cln eq 1) then begin               
                          ddd=Data2[(ipeakmax-floor(munit/2)):(ipeakmax+floor(munit/2))]
                          if(max(where(ddd gt 0)) gt -0.5) then thresh2=median(ddd[where(ddd gt 0)]) else thresh2=0
                         ; thresh2=median(ddd)
                          SD1=stddev(ddd[where(ddd lt thresh2)])
                          good=0
                          if(peakbroadness gt 20) then if(peakbroadness lt 10000) then  good=1
                          if( good eq 1) then if(countsmax lt thresh2+8*SD1) then good=0
                          if(good eq 1) then begin
                                  peaklist[peakcount,0]=m2t(ipeakmax,a,t0,ex,SampInt,/time) 
                                  peaklist[peakcount,1]=ipeakstart
                                  peaklist[peakcount,2]=ipeakend
                                  peaklist[peakcount,3]=peakbroadness
                                  peaklist[peakcount,4]=slopemax
                                  peaklist[peakcount,5]=slopemin
                                  peaklist[peakcount,6]=countsmax
                                  peaklist[peakcount,7]=ipeakmax ;timebin
                                  peaklist[peakcount,8]=countsmin
                                  PeakCount=PeakCount+1
                                  i=ipeakend-step+1
                          endif else if(ipeakmax gt 1) then i=ipeakmax-step+1
                  
                        ; print,[ good,m2t(ipeakmax,a,t0,ex,SampInt,/time), Thresh2, SD1,Thresh2+8* SD1,countsmax,20, peakbroadness,10000 ]
                        
                      ;   print, 'mass  |  TOF-bin  |   Bin-signal   | Good?   |  Broadness   | (countsmax-thresh2)/SD1'
                     ;    print, [m2t(i,a,t0,ex,SampInt,/time),ipeakmax,countsmax, good,peakbroadness, (countsmax-thresh2)/SD1]
                  endif else begin
                          peaklist[peakcount,0]=m2t(ipeakmax,a,t0,ex,SampInt,/time) 
                          peaklist[peakcount,1]=ipeakstart
                          peaklist[peakcount,2]=ipeakend
                          peaklist[peakcount,3]=peakbroadness
                          peaklist[peakcount,4]=slopemax
                          peaklist[peakcount,5]=slopemin
                          peaklist[peakcount,6]=countsmax
                          peaklist[peakcount,7]=ipeakmax ;timebin
                          peaklist[peakcount,8]=countsmin
                          PeakCount=PeakCount+1
                          i=ipeakend-step+1
                  endelse
          endif
  endwhile
  peaklist=peaklist[0:PeakCount-1,*]
  return,peaklist
end


function index, ppbdata,masses2, engnms, entdt, indexfile

   IndexPAR=ReadIndexFile(IndexFile) 
 

  engnms=strtrim(engnms,2)
  INDM=long(entdt[*,0]-entdt[*,0]) 
  INDS1=long(entdt[*,0]-entdt[*,0]) 
  INDS2=long(entdt[*,0]-entdt[*,0]) 
  INDS3=long(entdt[*,0]-entdt[*,0]) 
  INDS1M=0
  INDS2M=0
  INDS3M=0
  

  Dind=IndexPAR.VECTORIND
  if (max(str2vec(Dind[0])) eq -9999) then begin
        ;  time=entdt[*,where(engnms eq 'JulianDate')]
         ; step=(max(time)-min(time))/10
         ; for i=10,1,-1 do INDM[where(time le min(time)+step*i)]=100*i
          INDM=INDM+300
          if(strpos(strjoin(tag_names(indexpar),/single),'INTERVAL') gt -0.5) then MaxAvTime=STR2VEC(IndexPAR.INTERVAL)else MaxAvTime=-9999
  endif else begin
          OPCO=IndexPAR.CONDIND
          VAL=IndexPAR.VALUEIND
          IVAL=IndexPAR.IFIND
          IDEF=IndexPAR.ELSEIND
          MaxAvTime=STR2VEC(IndexPAR.INTERVAL)
          ; construct INDM  
          for  i=0,9 do begin
                  if(max(str2vec(OPCO[i])) gt 0.5 and max(str2vec(OPCO[i])) lt 5.5) then begin
                          indi=str2vec(Dind[i])
                          n=max(size(indi,/dimensions))
                          if (max(indi[0])*10000-floor(max(indi[0]))*10000 gt 0) then begin
                                  data=ppbdata[where(abs(masses2-max(indi[0])) lt 0.001),*] 
                                  if(n gt 1) then for j=1,n-1 do data=data+ppbdata[where(abs(masses2-max(indi[j])) lt 0.001),*] 
                          endif else begin
                                  data=entdt[*,indi[0]]
                                  if(n gt 1) then for j=1,n-1 do data=data+entdt[*,indi[j]]
                          endelse
                         ; ppbdata,masses2,
                          if (str2vec(idef[i]) ne -9999) then indM[*]=str2vec(idef[i])
                          if(max(str2vec(OPCO[i])) eq 1 ) then filter=where(data lt max(str2vec(VAL[i])))
                          if(max(str2vec(OPCO[i])) eq 2 ) then filter=where(data eq max(str2vec(VAL[i])))
                          if(max(str2vec(OPCO[i])) eq 3 ) then filter=where(data gt max(str2vec(VAL[i])))
                          if(max(str2vec(OPCO[i])) eq 4 ) then filter=where(data eq n*max(str2vec(VAL[i])))
                          if(max(str2vec(OPCO[i])) eq 5 ) then filter=where(data ge max(str2vec(VAL[i])))
     nn=max(size(filter,/dimensions))
     tol=9
     if(nn gt tol and max(indi[0])*10000-floor(max(indi[0]))*10000 gt 0) then begin
            if(filter[0+tol] gt filter[0]+tol+1) then filter[0]=-1 
            for zz=0,nn-1 do begin
                    if(filter[min([zz+tol,nn-1])] gt filter[zz]+tol+1) then if  ( filter[max([0,zz-tol])] lt filter[zz]-tol-1) then filter[zz]=-1                 
            endfor
            if  ( filter[nn-1-tol] lt filter[nn-1]-tol-1) then filter[nn-1]=-1
            filter=filter[where(filter gt -0.5)]  
           filter=[filter,filter[where(([0,filter]-[filter,0])+1 eq -1)]-1,filter[where(([0,filter]-[filter,0])+1 eq -2)]-1,filter[where(([0,filter]-[filter,0])+1 eq -2)]-2]
     endif                  
                          if(max(filter) gt -0.5) then indM[filter]=str2vec(IVAL[i])
                   endif
                   if(max(str2vec(OPCO[i])) gt 5.5 and max(str2vec(OPCO[i])) lt 8.5) then begin
                          indi=str2vec(Dind[i])
                          data=entdt[*,indi[0]]
                          n=max(size(indi,/dimensions))
                          if(n gt 2) then  for j=1,n-2 do  data=data+entdt[*,indi[j]]
                          if(max(str2vec(OPCO[i])) lt 7.5 ) then if(n ge 2) then data=data+abs(entdt[*,indi[n-1]]-1)
                          if(max(str2vec(OPCO[i])) eq 8 ) then if(n ge 2) then data=data-entdt[*,indi[n-1]]
                          if (str2vec(idef[i]) ne -9999) then indM[*]=str2vec(idef[i])
                          if(max(str2vec(OPCO[i])) eq 6 ) then filter=where(data eq n*max(str2vec(VAL[i])))
                          if(max(str2vec(OPCO[i])) eq 7 ) then filter=where(data ge max(str2vec(VAL[i])))
                          if(max(str2vec(OPCO[i])) eq 8 ) then filter=where(data gt max(str2vec(VAL[i])))
                          if(max(filter) gt -0.5) then indM[filter]=str2vec(IVAL[i])
                   endif
           endfor 
           ;hardcopy to INDM  
           ;print, indm 
           INDVALUE=IndexPAR.INDVALUE
           VALUES2ADD=IndexPAR.VALUES2ADD
           length=max(size(indM,/dimensions))
           for i=0,3 do begin
                  INDVAL2=max(str2vec(INDVALUE[i]))
                  if(INDVAL2 ne -9999) then begin
                          VALUES2ADD2=str2vec(VALUES2ADD[i])
                          indstart=where(indM[1:length-1] eq INDVAL2 and indM[0:length-2]-indM[1:length-1] ne 0)
                          
                          if(max(indstart) gt -0.5 or indM[0] eq INDVAL2) then begin
                                  if (max(indstart) eq -1) then indstart=fltarr(1)+1 else indstart=indstart+1
                                  
                                  indend=where(indM eq INDVAL2  and indM[0:length-2]-indM[1:length-1] ne 0) 
                                  if(max(indend) gt -1) then if(indstart[0] gt indend[0]) then indstart=[0,indstart]
                                  if(max(indend) gt -1) then if(indstart[max(size(indstart,/dimensions))-1] gt indend[max(size(indend,/dimensions))-1]) then indend=[indend,length-1]
                                  if(max(indend) eq -1) then  if(max(indstart) gt -1) then indend=length-1
                                  add=[VALUES2ADD2,intarr(900)]
                                  indM[indstart[0]+lindgen(1+indend[0]-indstart[0])]=indM[indstart[0]+lindgen(1+indend[0]-indstart[0])]+add[lindgen(1+indend[0]-indstart[0])]
                                  for r=1,max(size(indstart,/dimensions))-1 do indM[indstart[r]+lindgen(1+indend[r]-indstart[r])]=indM[indstart[r]+lindgen(1+indend[r]-indstart[r])]+add[lindgen(1+indend[r]-indstart[r])]
                          endif
                  endif           
          endfor  
          ; construct INDS  
      for  i=10,15 do begin
          if(max(str2vec(OPCO[i])) gt 0.5 and max(str2vec(OPCO[i])) lt 5.5) then begin
                  indi=str2vec(Dind[i])
                  data=entdt[*,indi[0]]
                  n=max(size(indi,/dimensions))
                  if(n gt 1) then for j=1,n-1 do data=data+entdt[*,indi[j]]
                  case i of
                          10:if (str2vec(idef[i]) ne -9999) then indS1M=str2vec(idef[i])
                          11:haha=1
                          12:if (str2vec(idef[i]) ne -9999) then indS2M=str2vec(idef[i])
                          13:
                          14:if (str2vec(idef[i]) ne -9999) then indS3M=str2vec(idef[i])
                          15:
                  endcase
                  if(max(str2vec(OPCO[i])) eq 1 ) then filter=where(data lt max(str2vec(VAL[i])))
                  if(max(str2vec(OPCO[i])) eq 2 ) then filter=where(data eq max(str2vec(VAL[i])))
                  if(max(str2vec(OPCO[i])) eq 3 ) then filter=where(data gt max(str2vec(VAL[i])))
                  if(max(str2vec(OPCO[i])) eq 4 ) then filter=where(data eq n*max(str2vec(VAL[i])))
                  if(max(str2vec(OPCO[i])) eq 5 ) then filter=where(data ge max(str2vec(VAL[i])))
                  case i of
                          10:if(max(filter) gt -0.5) then indS1[filter]=str2vec(IVAL[i])
                          11:if(max(filter) gt -0.5) then indS1[filter]=str2vec(IVAL[i])
                          12:if(max(filter) gt -0.5) then indS2[filter]=str2vec(IVAL[i])
                          13:if(max(filter) gt -0.5) then indS2[filter]=str2vec(IVAL[i])
                          14:if(max(filter) gt -0.5) then indS3[filter]=str2vec(IVAL[i])
                          15:if(max(filter) gt -0.5) then indS3[filter]=str2vec(IVAL[i])
                  endcase
          endif
          if(max(str2vec(OPCO[i])) gt 5.5 and max(str2vec(OPCO[i])) lt 8.5) then begin
                  indi=str2vec(Dind[i])
                  data=entdt[*,indi[0]]
                  n=max(size(indi,/dimensions))
                  if(n gt 2) then  for j=1,n-2 do  data=data+entdt[*,indi[j]]
                  if(max(str2vec(OPCO[i])) lt 7.5 ) then if(n ge 2) then data=data+abs(entdt[*,indi[n-1]]-1)
                  if(max(str2vec(OPCO[i])) eq 8 ) then if(n ge 2) then data=data-entdt[*,indi[n-1]]
                  case i of
                          10:if (str2vec(idef[i]) ne -9999) then indS1M=str2vec(idef[i])
                          11:
                          12:if (str2vec(idef[i]) ne -9999) then indS2M=str2vec(idef[i])
                          13:
                          14:if (str2vec(idef[i]) ne -9999) then indS3M=str2vec(idef[i])
                          15:
                  endcase
                  if(max(str2vec(OPCO[i])) eq 6 ) then filter=where(data eq n*max(str2vec(VAL[i])))
                  if(max(str2vec(OPCO[i])) eq 7 ) then filter=where(data ge max(str2vec(VAL[i])))
                  if(max(str2vec(OPCO[i])) eq 8 ) then filter=where(data gt max(str2vec(VAL[i])))
                  case i of
                          10:if(max(filter) gt -0.5) then indS1[filter]=str2vec(IVAL[i])
                          11:if(max(filter) gt -0.5) then indS1[filter]=str2vec(IVAL[i])
                          12:if(max(filter) gt -0.5) then indS2[filter]=str2vec(IVAL[i])
                          13:if(max(filter) gt -0.5) then indS2[filter]=str2vec(IVAL[i])
                          14:if(max(filter) gt -0.5) then indS3[filter]=str2vec(IVAL[i])
                          15:if(max(filter) gt -0.5) then indS3[filter]=str2vec(IVAL[i])
                  endcase
          endif
      endfor   
  endelse   
  s1=CREATE_STRUCT('m',indm,'s1',indS1,'s2',indS2,'s3',indS3,'s1M',indS1M,'s2M',indS2M,'s3M',indS3M,'T',MaxAvTime)

return, S1
end


function integrate, TofData, baseline,TimeRow,MassList,DeltaList,tolerance,a,t0,ex,aVector,t0Vector,exVector,name,jpeg, SampInt,extractions,poiscor
; cycle to cycle calculation of the count signal within the specified bounderies

; if jpeg is set to 1 images of raw data are saves including mass scale and integration boundaries. This is 
; important for checking the cor rectness and quality of routines, however it costs computation time and disk space.

;jpeg=0
if (max(where(DeltaList lt 0)) gt -0.5) then DeltaList[where(DeltaList lt 0)]=0.001
 if(getpar('CorrectionON') eq 1 and poiscor eq 0) then TofData=CorrPoissDead(TofData, extractions,sampint)
Q=SIZE(MassList, /DIMENSIONS)
mist=SIZE(TofData, /DIMENSIONS)
cps=FLTarr(Q,mist[1])
Dim=SIZE(cps, /DIMENSIONS)

;detect changes of the a, to, and ex parameters
parVec=aVector+100*t0Vector*100*exVector ;these factors sould avoid that e.g. differences in a and t0 cancel each other

ind=where([parVec[0],parVec[0:max(Dim[1]-2)]] -[parVec[0],parVec[1:max(Dim[1]-1)]] ne 0)
if(max(ind)gt 0) then ind=[0,ind] else ind=[0]
cnt=max(size(ind,/dimensions))
if (cnt gt 1) then npts=[ind[1:cnt-1]-ind[0:cnt-2],max(dim[1])-ind[cnt-1]] else npts=max(dim[1])-ind[cnt-1]
print,[(ind),(npts)]
for j=0,cnt-1 do begin ;loop over cycles in junks where the parVec does not change
;MassScale=((Timerow-t0Vector[j])/aVector[j])^(1/exVector[j])     ; time[i]

MassScale=m2t(Timerow, aVector[j],t0Vector[j],exVector[j],SampInt,/time)     ; time[i]

FOR i=0,Dim[0]-1 Do Begin ;loop over masses
                M=where(MassScale LT MassList[i]+DeltaList[i]/2)  
                N=where(MassScale GT MassList[i]-DeltaList[i]/2)
                if(min(N) lt 0) then N[where(N lt 0)]=0
                if(max(M) gt mist[0]-1) then M[where(M gt mist[0]-1)]=mist[0]-1
                if(min(N) gt max(M)) then N[where(N gt max(M))] = max(M)
             
                ii=indgen(npts[j])+ ind[j]   
                  
                Dat=TofData[min(N):max(M),ii]
                if(min(dat) lt 0) then Dat[where(dat lt 0)]=0
               ; cps[i,ii]=total(Dat,1)   
               if(max(baseline) eq-9999) then cps[i,ii]=total(Dat,1) else cps[i,ii]=total(Dat,1) - total(baseline[min(N):max(M)])*extractions   
               
           
             
                TofData[min(N),ii]=-9998 ;mark start of integration
                TofData[max(M),ii]=-9999 ;mark end of integration
      
          endfor
          
     
endfor
    print, ''      
    
    
 
;FOR i=0,Dim[0]-1 Do Begin ;loop over masses
;        Start =floor( t0 + a * (MassList[i]  - DeltaList[i]/2 - tolerance)^ex)  
;        Width = ceil((t0 + a * (MassList[i]  + DeltaList[i]/2 + tolerance)^ex)- Start)
;        Start2=where(Timerow EQ Start) ;TofData usually starts not at 0 but ~m19, so start 2 is the correct index value
;        Start=Start2[0]
;        Ende=Start+ Width  ;begin and end of the peak
;        Data =TofData[Start:Ende,*]
;        Time=Timerow[Start:Ende]
;        for j=0,Dim[1]-1 do begin ;loop over cycles
;                MassScale=((Time-t0Vector[j])/aVector[j])^(1/exVector[j])     ; time[i]
;                M=where(MassScale LT MassList[i]+DeltaList[i]/2)  
;                N=where(MassScale GT MassList[i]-DeltaList[i]/2)      
;                Dat=Data[min(N):max(M),j]
;                Dat=Dat[where(dat gt -1)]
;                cps[i,j]=total(Dat)
;                TofData[Start+min(N),j]=-9998 ;mark start of integration
;                TofData[Start+max(M),j]=-9999 ;mark end of integration
;       endfor
;endfor
    

if (jpeg eq 1) then begin
IF (N_ELEMENTS(name) ne 0 and max(d(MassList)) ge 2) then begin 
      maxpix=20000
      maxpix=long(maxpix)
      Start =floor( t0 + a * (MassList[2]  - DeltaList[2]/2 - tolerance)^ex)  
      Start2=where(Timerow EQ Start) ;TofData usually starts not at 0 but ~m19, so start 2 is the correct index value
      StartI=Start2[0]
      Ende =ceil((t0 + a * (MassList[Dim[0]-1 ]  + DeltaList[Dim[0]-1 ]/2 +tolerance)^ex))
      Ende2=where(Timerow EQ Ende) ;TofData usually starts not at 0 but ~m19, so start 2 is the correct index value
      EndeI=Ende2[0]
      for k=0,floor((EndeI-StartI)/(maxpix)) do begin
              StartI2=StartI+k*(maxpix)
              EndeI2=min([StartI-1+(k+1)*(maxpix),EndeI])
              ;TofData2=TofData[StartI2:EndeI2,*]
              ;Time=Timerow[StartI2:EndeI2]
              MassScale=m2t(Timerow,mean(aVector[0:20]),mean(t0Vector[0:20]),mean(exVector[0:20]),SampInt)
              length=max(size(MassScale[StartI2:EndeI2],/dimensions))
              for i=long(StartI2+1),StartI2+length-101 do begin
                      if(floor(2*MassScale[i]) gt floor(2*MassScale[i-1])) then begin
                              if(floor(MassScale[i]) eq floor(MassScale[i-1])) then begin
                                      sieb=drucksieb(string(floor(MassScale[i])+0.5,  FORMAT='(F5.1)'),2)
                                      dimsieb=size(sieb,/dimensions)
                                      dat=TofData[i:i+dimsieb[0]-1,22:22+dimsieb[1]-1]
                                      dat[where(sieb eq -9999)]=-9998
                                      TofData[i:i+dimsieb[0]-1,22:22+dimsieb[1]-1]=dat  
                                       TofData[i,0:20]=-9998
                              endif
                      endif
             endfor
            filter=where(TofData EQ -9998)
            if(max(filter) gt -1) then TofData[filter]=max(TofData)
            filter=where(TofData EQ -9999)
            if(max(filter) gt -1) then TofData[filter]=max(TofData)*0.1
              loadct,20, rgb_table=colortable
              dim= size(TofData[StartI2:EndeI2,*],/dimensions)
              write_jpeg, strjoin([name,string(k),'.jpeg'],''),reform(colortable[bytscl(alog10(temporary(TofData[StartI2:EndeI2,*])+0.1)),*],dim[0],dim[1],3), true=3
            
           ; saveImage, strjoin([name,string(k),'.jpeg'],''),alog10(TofData[StartI2:EndeI2,*]+0.1),20   
      endfor
      
endif
endif

Return, cps
end




function masslib , extended=extended, sulfate=sulfate, cal=cal

if (keyword_set(extended)) then ooo=16 else ooo=5
if (keyword_set(extended)) then nnn=2 else nnn=0
if (keyword_set(extended)) then Clll=0 else Clll=0
if (keyword_set(sulfate)) then sss=1 else sss=0
if (keyword_set(cal)) then begin 
        SRI=1
        ooo=5
        nnn=0
        Clll=0
        sss=0
endif else SRI=0
; create a library of protonated ion masses
C12=12
C13=13.003355
H1=1.007825
O16=15.994915
O17=16.999131
O18=17.99916
N14=14.003074
N15=15.000108
H_pos=1.007276467
e_neg=-0.000548533
Cl35=34.968852
Cl37=36.965903
F=18.998403
S32O4=31.97207+4*O16
S34O4=33.967866+4*O16


;primary/inorganic ions
;m21
n=[0,0,2,0,0,1,0,0,1,0,0,0,0,0,0]
lib=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
;m39
n=[0,0,4,1,0,1,0,0,1,0,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
;O2+
n=[0,0,0,2,0,0,0,0,0,1,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
;16O18O+
n=[0,0,0,1,0,1,0,0,0,1,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
;NO+
n=[0,0,0,1,0,0,1,0,0,1,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
;N2H+
n=[0,0,0,0,0,0,2,0,1,0,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
;NH4+
n=[0,0,3,0,0,0,1,0,1,0,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
;NH3NH4+
n=[0,0,6,0,0,0,2,0,1,0,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
;HNO3H+
n=[0,0,1,3,0,0,1,0,1,0,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]

;NO2+
n=[0,0,0,2,0,0,1,0,0,1,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]

;15NO2+
n=[0,0,0,2,0,0,0,1,0,1,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]


;NO18O+
n=[0,0,0,1,0,1,1,0,0,1,0,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]

;CCl3+
n=[1,0,0,0,0,0,0,0,0,1,3,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[1,0,0,0,0,0,0,0,0,1,2,1,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[0,1,0,0,0,0,0,0,0,1,3,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[0,1,0,0,0,0,0,0,0,1,2,1,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[1,0,0,0,0,0,0,0,0,1,1,2,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]

;C20H15ClO4
n=[20,0,15,4,0,0,0,0,1,0,1,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[19,1,15,4,0,0,0,0,1,0,1,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[20,0,15,4,0,0,0,0,1,0,0,1,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[19,1,15,4,0,0,0,0,1,0,0,1,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]


;C6H5ClO3
n=[6,0,5,3,0,0,0,0,1,0,1,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[5,1,5,3,0,0,0,0,1,0,1,0,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[6,0,5,3,0,0,0,0,1,0,0,1,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]
n=[5,1,5,3,0,0,0,0,1,0,0,1,0,0,0]
entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
lib=[[lib],[entry]]



;all other CHNOCl
for c=1,40 do for h=c-9,c+1 do for o=0,ooo do for nn=0,nnn do for Cll=0,Clll do for ss=0,sss do if (h gt -0.5) then if(nn ge 1 or h gt 0.5) then begin
        n=[c,0,2*h+nn,o,0,0,nn,0,1,0,Cll,0,0,ss,0]
        entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
        lib=[[lib],[entry]]
        if (keyword_set(cal)) then begin ; carge transfer (SRI-mode)
                n=[c,0,2*h+nn,o,0,0,nn,0,0,1,Cll,0,0,ss,0]
                entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
                lib=[[lib],[entry]]
        endif
        if (keyword_set(extended)) then begin ;13C isotope..
                n=[c-1,1,2*h+nn,o,0,0,nn,0,1,0,Cll,0,0,ss,0]
                entry=[C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9],n]
                lib=[[lib],[entry]]     
        endif   
endif


lib=lib[*,sort(lib[0,*])]
print, size(lib,/dimensions)

return, lib
end


function overlap, masslist, data, name, path

mistt=systime(1)
lenPeaks=max(size(masslist,/dimensions))
dims=size(data,/dimensions)
if(max(dims[0]) ne lenPeaks) then begin 
      data=transpose(data)
      lenCyc=max(dims[0])
endif else lenCyc=max(dims[1])
if (max(size(name,/dimensions)) le 1) then name=strarr(lenCyc)+name
nam='xxx'
DataCorr=fltarr(lenPeaks,lenCyc)
    
for j=0,lenCyc-1 do begin ;loop over averaged cycles 
;for j=148,149 do begin
        if(name[j] ne nam) then begin
                nam=name[j]
                reso=readfloat(path+'FileInfo\Time2MassPAR\'+nam+'Par2.fdt')
                reso=reso[8]
                PeakData=peaktable(masslist,reso)
                FWHM=PeakData[*,3]*2*SQRT(2*ALOG(2))
                Peakshape=readfloat(path+'FileInfo\PeakShape\'+nam+'PeakShape.fdt')
                x7=Peakshape[*,0]
                dataX3=Peakshape[*,1]
                bb1=(PeakData[*,1]-PeakData[*,0])/FWHM 
                bb2=(PeakData[*,2]-PeakData[*,0])/FWHM 
                bb1_=([PeakData[*,1],0]-[0,PeakData[*,0]])/[FWHM ,1]
                bb2_=(PeakData[*,2]-[PeakData[1:*,0],10000])/FWHM 
                b1=bb1
                b2=bb2
                b1_=bb1_
                b2_=bb2_
                for i=0,max(size(b1,/dimensions))-1 do begin 
                        b1[i]=total(dataX3[where(x7 lt bb1[i] )])/total(dataX3)       
                        b2[i]=total(dataX3[where(x7 gt bb2[i] )])/total(dataX3) 
                        filter=where(x7 gt bb1_[i] )      
                        if(max(filter) gt -0.5) then b1_[i]=total(dataX3[filter])/total(dataX3) else b1_[i]=0      
                        filter=where(x7 lt bb2_[i] )      
                        if(max(filter) gt -0.5) then b2_[i]=total(dataX3[filter])/total(dataX3) else b2_[i]=0         
                endfor
                dim=max(size(b1,/dimensions))
                A=fltarr(dim,dim)
                for i=0,dim[0]-1 do begin
                        if(i gt 0.5) then A[i,i-1]=b2_[i-1]
                        A[i,i]=1-b1[i]-b2[i]
                        if(i lt dim-1.5) then A[i,i+1]=b1_[i+1]
                endfor
                print,"Matrix A built, dims:", size(A,/dimensions),"elapsed time:",  systime(1)-mistt
                C=A
                LUDC, C, INDEX   ;3.9s pro cycle
        endif
        B=Data[*,j]
        B2=fltarr(dim)
        B2[where(B gt -9999)]=B[where(B gt -9999)]
        S=LUSOL(C, INDEX, B2)  
        filter=where(S lt 0)    
        DataCorr[*,j]=S 
        if(float(j)/200 eq floor(float(j)/200)) then print, 'CyCle: ',j, ' time =  ', systime(1)-mistt
endfor


if(max(dims[0]) ne lenPeaks) then DataCorr=transpose( DataCorr)


return, DataCorr
end




function PS1,event,SumSpectrum, SampInt,duration, cycles, extractions,destfold,name,instrument
  
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE='Peaks & time2mass:'
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), gET_VALUE=info
    
  
    x=lindgen(max(size(SumSpectrum,/dimensions) )-1)
    name=reform(name)
    lib=masslib(/cal) &  lib=lib[0,*]
  ; print, d(lib)
  
    if(strpos(instrument,'TOF1000') eq 0) then begin
            a=getpar('default_a_1000')
            t0=getpar('default_t0_1000')
    endif
    if(strpos(instrument,'TOF8000') eq 0) then begin
            a=getpar('default_a_8000')
            t0=getpar('default_t0_8000')
    endif
    print, [0,0,0,a,t0]
    peaklist=DetectPeaks(SumSpectrum,a , t0, 0.5,SampInt)
    par=CalCrude(peaklist,SampInt,lib,instrument)
    a=par.a
    t0=par.t0
    ex=0.5
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE=[info,'CalCrude:','a='+string(a),'t0='+string(t0),'ex=0.5']
   ; print, d(lib)
 
    Top16=par.Top16
if (a ne -9999) then begin  ; if calCrude failed (a=-9999), do non of the remaining stuff... 
    peaklist=DetectPeaks(SumSpectrum, a, t0, ex,SampInt,/clean)
    ex=0.5
    a_raw=a
    t0_raw=t0
    ex_raw=0.5
    
    
    
    reso=mean(peaklist[*,3]);default resolution  
    masses=m2t(peaklist[*,7],a_raw,t0_raw,0.5,SampInt,/time)
    mm_1=[21.022, 39.0327,30.994, 33.9935, 47.9966]
    mm_2=[487, 242, 269, 242, 242]
    Psig=0
    for r=0,4 do begin
            rr2=floor(m2t([mm_1[r]-max([0.005, mm_1[r]/reso]),mm_1[r]+max([0.005, mm_1[r]/reso])],a,t0,ex,SampInt))
            sig2=(corrtr(79)/corrtr(mm_1[r]))*mm_2[r]*total(SumSpectrum[rr2[0]:rr2[1]])/duration
            pp=floor((rr2[1]-rr2[0])/2)
            if(floor(1+(rr2[1]-rr2[0])/2) eq pp) then qq=pp-1 else qq=pp
            bk2=(corrtr(79)/corrtr(mm_1[r]))*mm_2[r]*total([SumSpectrum[-1-qq+rr2[0]:rr2[0]-1],SumSpectrum[1+rr2[1]:rr2[1]+1+pp]])/duration
            Psig=[Psig,sig2-bk2]
    endfor
    Psig=Psig[1:*]
   ;if(where(Psig eq max(Psig)) lt 1.5 ) then mode='H3O+' else if(where(Psig eq max(Psig)) eq 3 ) then mode='O2+' else mode='NO+' 
   if(where(Psig eq max(Psig)) lt 1.5 ) then mode=0 else if(where(Psig eq max(Psig)) eq 3 ) then mode=9 else mode=5 
    
    
      Flib=-1
      if( mode eq 0) then begin
              Flib=where(2*floor(floor(lib+0.4)/2) ne floor(lib+0.4) or lib lt 50);selects odd masses
              if(max(Flib) gt 0) then lib2=lib[Flib] 
      endif else lib2=lib
      
     
      
    
  ;    print, d(lib)
 
    fineCal=CalFine(peaklist[*,7], lib2, a,t0, SampInt,mode,instrument)
    a_fine=max(fineCal.a)
    t0_fine=max(fineCal.t0)
    ex_fine=max(fineCal.ex)
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), gET_VALUE=info
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE=[info,'','CalFine:','a='+string(a_fine),'t0='+string(t0_fine),'ex='+string(ex_fine)]
   
    ;   print, d(lib)
   
    cal3=Cal3pt(peaklist,a_fine,t0_fine,ex_fine,sampint,lib2,mode,instrument)
    a3=max(cal3.a)
    t03=max(cal3.t0)
    ex3=max(cal3.ex)
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), gET_VALUE=info
    WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE=[info,'','Cal3pt:','a='+string(a3),'t0='+string(t03),'ex='+string(ex3)]
   
    tst=testSCALE(m2t(peaklist[*,7],a_raw,t0_raw,0.5, SampInt), lib2,instrument)
    scorRaw=(tst.scorppm)
    tst=testSCALE(m2t(peaklist[*,7],a_fine,t0_fine,ex_fine, SampInt), lib2,instrument)
    scorParVar=(tst.scorppm)
    tst=testSCALE(m2t(peaklist[*,7],a3,t03,ex3, SampInt), lib2,instrument)
    scor3=(tst.scorppm)
    if(scorRaw gt scorParVar and scorRaw gt scor3) then begin
            a=a_raw
            t0=t0_raw
            ex=ex_raw
    endif
     if(scor3 gt scorParVar and scor3 gt scorRaw) then begin
            a=a3
            t0=t03
            ex=ex3
    endif
      
    if(scorParVar ge scor3 and scorParVar ge scorRaw) then begin
            a=a_fine
            t0=t0_fine
            ex=ex_fine
    endif
    
  
  
 ;;;;;;;;;;;;;;;;;;
 ;;;;;; Baseline signal
 ;;;;;;;;;;;;;;;;;;
if (getpar('PeakAnalysis') eq 1) then begin    
    
    
   times=peaklist[*,7]
   masses=m2t(times,a,t0,ex,SampInt)
   extr=float(cycles)*float(extractions)
   SumSpectrumSM=smooth(SumSpectrum,5*floor(0.55+1e-10/sampint))
   endmass=m2t(max(size(SumSpectrum,/dimensions)), a,t0,ex,SampInt)

;;; calculate baseline  signal

pts=max(size(SumSpectrum,/dimensions))
baseline=fltarr(pts)
oldt=0
n_SEG=floor(600*1e-10/SampInt)
for iii=0,floor(pts/n_SEG)-1 do begin
    newt=long(iii+1)*n_SEG
    SEG=SumSpectrum[indgen(newt-oldt)+oldt]
    for iv=0,6 do begin
            nn=max(size(SEG,/dimensions))
             i_max=where(SEG eq max(SEG)) & i_max=i_max[0]
             ; delete 60 pts around i_max (maximum signal), totally 7 times i.e. 70% of the segment is deleted
            if(i_max le n_SEG/20) then SEG=SEG[n_SEG/10:*] else if(i_max ge nn-n_SEG/20) then SEG=SEG[lindgen(nn-n_SEG/10)] else SEG=SEG[[indgen(i_max-n_SEG/20),i_max+n_SEG/20+indgen(nn-i_max-n_SEG/20)]]
    endfor
  ;  baseline[oldt:newt-1]=mean(SEG[where(SEG le max([quantile(SEG,0.5),2]))])
    baseline[oldt:newt-1]=mean(SEG)
    oldt=newt
endfor
baseline[where(baseline eq 0)]=min(baseline[where(baseline gt 0)])
baselineSM=CONVOL(baseline, SAVGOL(floor(1.5*n_SEG),floor(1.5*n_SEG), 0,1 ), /EDGE_TRUNCATE)

WINDOW, 4, xsize=1150,ysize=700
 !P.MULTI = [0,1,4] 
 !x.oMARGIN=[8,8]
  !Y.MARGIN = [3, 1]
 DEVICE, DECOMPOSED = 0 
 loadct,5
 xLim=floor([2e4,14e4,22e4,28e4]*1e-10/(SampInt))
 if(max(xLim) gt 0.9*pts) then xLim=floor(0.9*pts*xLim/max(xLim))
 xLim=[xLim,pts-1]
 yLim=[1,13,13,13]
 for kkk=0,3 do begin
        if(kkk eq 3) then  !Y.MARGIN = [4, 0] else  if(kkk eq 2) then  !Y.MARGIN = [3, 1] else  !Y.MARGIN = [2, 1]
        xxx=findgen(xLim[kkk+1]-xLim[kkk])+xLim[kkk]
        yyy=SumSpectrum[xLim[kkk]:xLim[kkk+1]]/extr
        yyy2=baseline[xLim[kkk]:xLim[kkk+1]]/extr
        if(kkk lt 0.5) then  plot,xxx,yyy,xstyle=1,ystyle=1,yrange=[min(baseline/extr),5*max(yyy2)],color=0,ylog=1,background=-1,charsize=3;, charthick=2
        if(kkk eq 1) then  plot,xxx[where(yyy gt 0)],yyy[where(yyy gt 0)],xstyle=1,ystyle=1,yrange=[0,5*max(yyy2)],color=0,background=-1,charsize=3;, charthick=2
        if(kkk eq 2) then  plot,xxx[where(yyy gt 0)],yyy[where(yyy gt 0)],ytitle='                        counts per TOF-bin per extraction ('+string(extr/duration,format='(I5)')+' extractions per second)',xstyle=1,ystyle=1,yrange=[0,10*max(yyy2)],color=0,background=-1,charsize=3;, charthick=2
        if(kkk eq 3) then  plot,xxx[where(yyy gt 0)],yyy[where(yyy gt 0)],xtitle='TOF-bin number ('+strtrim(string(sampint),2)+' second bins)',xstyle=1,ystyle=1,yrange=[0,10*max(yyy2)],color=0,background=-1,charsize=3;, charthick=2
        oplot,xxx,yyy2,color=50, thick=4
        oplot, xxx,baselineSM[xLim[kkk]:xLim[kkk+1]]/extr,color=115, thick=3
 endfor
  !Y.MARGIN = [4, 2]
 for kk=0,max(size(name,/dimensions))-1 do  write_jpeg, destfold+'FileInfo/Baseline/' +'Baseline_'+name[kk]+'.jpg',tvrd(0,true=1), true=1
 ;---MB WDELETE, 4
 !x.oMARGIN=[2,2]
   
         
 ;;;;;;;;;;;;;;;;;;
 ;;;;;; peak shape
 ;;;;;;;;;;;;;;;;;;
 
 resoD=mean(peaklist[*,3]);default resolution
 reso=resoD  
 mul=40
 PtsPerFWHM=100
 minsig=100; --MB getpar('MinSig') ;Default=800
 m59=fltarr(mul*PtsPerFWHM+1)
 m21=fltarr(mul*PtsPerFWHM+1)
 savgolFilter = SAVGOL(15,15, 0,4 )
 maxi=0
 for iii=0,max(size(masses,/dimensions))-1 do if(SumSpectrumSM[times[iii]] gt minsig) then  maxi=[maxi,SumSpectrumSM[times[iii]]]
 maxi=maxi[1:*]
 maxsig=max(maxi)
 if (max(size(maxi[where(maxi lt maxsig/100 or maxi lt max(minsig)*10)],/dimensions)) ge 2) then begin
    
 
 for kk=0,1 do begin
      x7=findgen(mul*PtsPerFWHM+1)
      x7=2*mul*x7/max(x7)-mul
      y=fltarr(mul*PtsPerFWHM+1,max(size(maxi[where(maxi lt maxsig/100 or maxi lt max(minsig)*10)],/dimensions)))
      MBm=fltarr(max(size(maxi[where(maxi lt maxsig/100 or maxi lt max(minsig)*10)],/dimensions)))
      ;y_part=fltarr(round(max(size(masses,/dimensions))/50+0.5), mul*PtsPerFWHM+1,max(size(maxi[where(maxi lt maxsig/100 or maxi lt max(minsig)*10)],/dimensions)))
      counter=0
      ;counter_part=0
      ;for lll=0,max(size(masses,/dimensions))-50, 50 do begin
        ;counter2=0
        for iii=0,max(size(masses,/dimensions))-1 do begin
          ind=m2t([masses[iii]-mul*masses[iii]/reso,masses[iii]+mul*masses[iii]/reso], a, t0, ex,SampInt)
          if(evenodd(round(masses[iii])) eq 1 and (SumSpectrumSM[times[iii]] gt minsig) and (SumSpectrumSM[times[iii]] lt maxsig/100 or SumSpectrumSM[times[iii]] lt max(minsig)*10)) then begin
            y1=SumSpectrum[indgen(max(ind)-min(ind))+min(ind)]-baselineSM[indgen(max(ind)-min(ind))+min(ind)]; subtract sumspec
            y1=y1/(SumSpectrumSM[times[iii]]-baselineSM[times[iii]]); normalize
            y1=INTERPOLATE(y1, (max(ind)-min(ind))*findgen(mul*PtsPerFWHM+1)/(mul*PtsPerFWHM+1))
            y[*,counter]=y1
            MBm[counter]=masses[iii]
            ;y_part[counter_part,*,counter2]=y1
            counter=counter+1
            ;counter2=counter2+1
          endif
          if( abs(59.049-masses[iii]) lt 0.01 OR abs(21.022-masses[iii]) lt 0.01) then begin
            y1=SumSpectrum[indgen(max(ind)-min(ind))+min(ind)]-baselineSM[indgen(max(ind)-min(ind))+min(ind)]; subtract sumspec
            y1=y1/(SumSpectrumSM[times[iii]]-baselineSM[times[iii]]); normalize
            y1=INTERPOLATE(y1, (max(ind)-min(ind))*findgen(mul*PtsPerFWHM+1)/(mul*PtsPerFWHM+1))
            if(abs(59.049-masses[iii]) lt 0.01) then m59=y1
            if(abs(21.022-masses[iii]) lt 0.01) then m21=y1
          endif
        endfor
        ;counter_part=counter_part+1
      ;endfor
      ;parts = counter_part-1
      if(kk eq 0) then begin
        reso=0
              dims=size(y,/dimensions)
              ;plot, x7, y[*,0],color=0,background=-1, xrange=[-4,4], yrange=[-0.2,1.2], ytitle='normalized signal',xtitle='relative m/z [multiples of estimated FWHM]', charsize=1.2, charthick=1.5
              ;for iii=0,dims[1]-1 do oplot , x7, y[*,iii] ,color=145     
              quant=findgen(100)/100
              quant=quant[3:52]
              fit2=fltarr(mul*PtsPerFWHM+1,50)
              for iii=0,mul*PtsPerFWHM do fit2[iii,*]=quantile(y[iii,*], quant)

              ;---MB cntmist=0
              for lll=0,49 do begin
                      fit=CONVOL(fit2[*,lll], savgolFilter, /EDGE_TRUNCATE)
                      fit=fit/max(fit)
                      fit3=fit[indgen(mul*PtsPerFWHM)]
                      ind2=where(fit3 gt 0.5)
                      dfit=fit[min(ind2)]-fit[min(ind2)-1]
                      dx=x7[min(ind2)]-x7[min(ind2)-1]
                      x1=x7[min(ind2)-1]+(0.5-fit[min(ind2)-1])*dx/dfit
                      dfit=fit[max(ind2)]-fit[max(ind2)+1]
                      dx=x7[max(ind2)+1]-x7[max(ind2)]
                      x2=x7[max(ind2)]+(fit[max(ind2)]-0.5)*dx/dfit
                      FWHM=x2-x1
                      reso=[reso,resoD/FWHM]
                   ;  if(min(abs([0.1,0.2,0.3,0.4,0.5]-quant[lll])) eq 0) then begin
                   ;  oplot , x7, fit ,color=0 ,thick=3 
                   ;  xyouts, -3.5,0.9-cntmist, 'Quantile='+string(quant[lll],format='(f4.2)')+', FWHM='+string(resoD/FWHM,format='(I4)'),color=0, charsize=1.2, charthick=1.5
                   ;  cntmist=cntmist+0.07
                   ;  endif
              endfor
              
              ;--------- MB
              m_low = min(MBm)
              m_high = max(MBm)
              MBresolution=0
              MBresolutionM=0
              for mbi=0,19 do begin
                mlow=m_low+mbi*(m_high-m_low)/20
                mhigh=mlow+(m_high-m_low)/20
                MBM_indizes = where(MBm GT mlow and MBm LT mhigh, count)
                if count GT 10 then begin
                  print, mbi
                  MBfit2=fltarr(mul*PtsPerFWHM+1)
                  for iii=0,mul*PtsPerFWHM do MBfit2[iii]=quantile(y[iii,MBm_indizes], 0.1)
                  MBfit=CONVOL(MBfit2, savgolFilter, /EDGE_TRUNCATE)
                  MBfit=MBfit/max(MBfit)
                  MBfit3=MBfit[indgen(mul*PtsPerFWHM)]
                  ind2=where(MBfit3 gt 0.5)
                  MBdfit=MBfit[min(ind2)]-MBfit[min(ind2)-1]
                  dx=x7[min(ind2)]-x7[min(ind2)-1]
                  x1=x7[min(ind2)-1]+(0.5-MBfit[min(ind2)-1])*dx/MBdfit
                  MBdfit=MBfit[max(ind2)]-MBfit[max(ind2)+1]
                  dx=x7[max(ind2)+1]-x7[max(ind2)]
                  x2=x7[max(ind2)]+(MBfit[max(ind2)]-0.5)*dx/MBdfit
                  FWHM=x2-x1
                  MBresolution=[MBresolution,resoD/FWHM]
                  MBresolutionM=[MBresolutionM,(mlow+mhigh)/2]
                endif
              endfor
              MBresolution=MBresolution[1:*]
              MBresolutionM=MBresolutionM[1:*]

                  
            ;   for kk=0,max(size(name,/dimensions))-1 do  write_jpeg, destfold+'FWHM2_'+name[kk]+'.jpg',tvrd(0,true=1), true=1
             ; WDELETE, 5
             
              reso=reso[1:*]
              d_reso=deriv(quant, reso)
              i_quant=min([30,max([7,where(d_reso[0:30] lt median(d_reso[35:49])*3)])])
              WINDOW, 6, xsize=800,ysize=600
              !P.MULTI = [0,1,2] 
              DEVICE, DECOMPOSED = 0 
              loadct,5
              plot, quant, reso,color=0,background=-1
              oplot, [quant[i_quant],quant[i_quant]],[-1e8,1e8], color=40
              ;----MB plot, quant, d_reso ,color=0,background=-1
              plot, MBresolutionM, MBresolution ,color=0,background=-1
              ;plot, reso_m, reso,color=0,background=-1 ;---MB
              ;----MB oplot, [quant[i_quant],quant[i_quant]],[-1e8,1e8], color=40
              for kk=0,max(size(name,/dimensions))-1 do  write_jpeg, destfold+'FileInfo/PeakShape/'+'FWHM(quantile)2_'+name[kk]+'.jpg',tvrd(0,true=1), true=1
              WDELETE, 6
      
              quant=quant[i_quant]
              reso=reso[i_quant]
      endif
      if(kk eq 1) then begin
              fit=fltarr(mul*PtsPerFWHM+1)
              ;parts = round(max(size(masses,/dimensions))/50+0.5)-1; ---MB
              ;peakShapes=fltarr(parts,mul*PtsPerFWHM+1); ---MB
              for iii=0,mul*PtsPerFWHM do  fit[iii,*]=quantile(y[iii,*], quant)
              ;for mbi=0,parts-1 do begin; ---MB
              ;  for iii=0,mul*PtsPerFWHM do  peakShapes[mbi,iii,*]=quantile(y_part[mbi,iii,*], quant)
              ;endfor
      endif 
 endfor
 dataX3=CONVOL(fit, savgolFilter, /EDGE_TRUNCATE)
 dataX3= dataX3/max(dataX3)
 indmax=where(dataX3 eq max(dataX3))
 for iii=min(indmax)-10,0,-1 do if(dataX3[iii] gt dataX3[iii+1]) then dataX3[iii]=dataX3[iii+1]*0.996
 for iii=max(indmax)+10,mul*PtsPerFWHM do if(dataX3[iii] gt dataX3[iii-1]) then dataX3[iii]=dataX3[iii-1]*0.996
 if(max(where(dataX3 lt 0)) gt -0.5) then dataX3[where(dataX3 lt 0)]=0 
 ind0=where(abs(x7) lt 0.000001)
 if(indmax gt ind0) then begin   ; put the max to center...
      tt=indmax-ind0
      dataX3=[dataX3[tt:*],fltarr(tt)]
      fit=[fit[tt:*],fltarr(tt)]
      m59=[m59[tt:*],fltarr(tt)]
      m21=[m21[tt:*],fltarr(tt)]
      y=[y[tt:*,*],fltarr(tt,dims[1])]
 endif
 if(indmax lt ind0) then begin
      tt=-indmax+ind0
      dataX3=[fltarr(tt),dataX3[0:mul*PtsPerFWHM-tt]]
      fit=[fltarr(tt),fit[0:mul*PtsPerFWHM-tt]]
      m59=[fltarr(tt),m59[0:mul*PtsPerFWHM-tt]]
      m21=[fltarr(tt),m21[0:mul*PtsPerFWHM-tt]]
      y=[fltarr(tt,dims[1]),y[0:mul*PtsPerFWHM-tt,*]]
      
 endif



 m59=m59/max(m59[1950:2050]); i.e. this range is one time FWHM
 m21=m21/max(m21[1950:2050]); i.e. this range is one time FWHM
 fit=fit/max(fit[1950:2050])
 dataX3=dataX3/max(dataX3[1950:2050])       
 ;WINDOW, 5, xsize=1200,ysize=800 --- MB
 WINDOW, 5, xsize=1200,ysize=800
!P.MULTI = [0,2,1] 
 DEVICE, DECOMPOSED = 0 
 loadct,5
 plot,[-mul/10,mul/10],[0,1],color=0,background=-1,psym = 4,yrange=[0, 1.4], ytitle='relative intensity',xtitle='relative m/z [multiples of FWHM]', thick=2,charsize=1.5, charthick=2
 oplot,[-mul,mul],[0,1],color=-1,psym = 4, thick=2
 oplot,[0,0],[0.0001,1.2],color=0
 oplot,[-mul,mul],[0.5,0.5],color=0
 for iii=0,dims[1]-1 do oplot , x7, y[*,iii] ,color=145     
 
 ;oplot, x7,m59+0.0001, color=110, thick=5
 oplot, x7,fit, color=95, thick=5
 oplot, x7,dataX3+0.0001, color=50, thick=2
 xyouts,-3.5,1.4,Name,charsize=1.3, charthick=2, color=1
 ;xyouts,-3.5,1.34,'mass res. (FWHM): '+string(reso,format='(I4)'),charsize=1.3, charthick=2, color=1
 xyouts,-3.5,1.31,string(max(size(y[0,*],/dimensions)),format='(I4)')+' individual peaks',charsize=1.3, charthick=2, color=125
 xyouts,-3.5,1.25,string(quant,format='(f4.2)')+'-quantile of'+string(max(size(y[0,*],/dimensions)),format='(I4)')+' peaks',charsize=1.3, charthick=2, color=95
 xyouts,-3.5,1.19,'Savitzky-Golay smooth filt',charsize=1.3, charthick=2, color=50
 sigma=1/(2*SQRT(2*ALOG(2)))        
 plot,[-mul/10,mul/10],[0,1],color=0,background=-1,ytitle='relative intensity',xtitle='relative m/z [multiples of FWHM]',psym = 4, thick=2,charsize=1.5, charthick=2, yrange=[0,1.4]
 oplot,[-mul/10,mul/10],[0,1],color=-1,psym = 4, thick=2
 oplot,[4*sigma,4*sigma],[0,1.08],color=0
 oplot,[2*sigma,2*sigma],[0,1.08],color=0
 oplot,[0,0],[0.9,1.02],color=0
 oplot,[-2*sigma,-2*sigma],[0,1.08],color=0
 oplot,[-4*sigma,-4*sigma],[0,1.08],color=0
 oplot,[-mul,mul],[0.5,0.5],color=0
 oplot, x7,m59, color=175, thick=5
 oplot, x7,m21, color=135, thick=5
 oplot, x7,fit, color=95, thick=5
 oplot, x7,dataX3, color=50, thick=2
 ;xyouts,-3.5,1.4,Name,charsize=1.3, charthick=2, color=1
 xyouts,-3.5,1.4,'mass res. (FWHM): '+string(reso,format='(I4)'),charsize=1.3, charthick=2, color=1
 xyouts,-3.5,1.19,'Savitzky-Golay smooth filt',charsize=1.3, charthick=2, color=50
 xyouts,-3.5,1.25,string(quant,format='(f4.2)')+'-quantile of'+string(max(size(y[0,*],/dimensions)),format='(I4)')+' peaks',charsize=1.3, charthick=2, color=95
 xyouts,-3.5,1.31,'m/z 59.049',charsize=1.3, charthick=2, color=175
 xyouts,-1,1.31,'m/z 21.022',charsize=1.3, charthick=2, color=135
 xyouts,-3.1,1.09,'%  at +/- 2 & 4 sig:',charsize=1.3, charthick=2, color=1
 xyouts,-8*sigma,1.04,string(100*total(dataX3[where(x7 lt -4*sigma )])/total(dataX3),format='(F4.1)'),charsize=1.3, charthick=2, color=1
 xyouts,-4.4*sigma,1.04,string(100*total(dataX3[where(x7 ge -4*sigma and x7 lt -2*sigma)])/total(dataX3),format='(F4.1)'),charsize=1.3, charthick=2, color=1
 xyouts,-1.5*sigma,1.04,string(100*total(dataX3[where(x7 ge -2*sigma and x7 lt 2*sigma)])/total(dataX3),format='(F4.1)'),charsize=1.3, charthick=2, color=1
 xyouts,1.6*sigma,1.04,string(100*total(dataX3[where(x7 ge 2*sigma and x7 lt 4*sigma)])/total(dataX3),format='(F4.1)'),charsize=1.3, charthick=2, color=1
 xyouts,4.5*sigma,1.04,string(100*total(dataX3[where(x7 ge 4*sigma)])/total(dataX3),format='(F4.1)'),charsize=1.3, charthick=2, color=1
  for kk=0,max(size(name,/dimensions))-1 do  write_jpeg, destfold+'FileInfo\PeakShape\'+'PeakShape_'+name[kk]+'.jpg',tvrd(0,true=1), true=1
 WDELETE, 5
 endif else begin
            reso=getpar('DefaultRes')
         mul=40
        PtsPerFWHM=100
        x7=findgen(mul*PtsPerFWHM+1)
        x7=2*mul*x7/max(x7)-mul
        zz=x7* 2*SQRT(2*ALOG(2)) 
        dataX3=exp(-0.5*zz^2)  
 endelse

 endif else begin ;if(getpar('PeakAnalysis') eq 1)
        reso=getpar('DefaultRes')
        baselinesm=[-9999,-9999]
        mul=40
        PtsPerFWHM=100
        x7=findgen(mul*PtsPerFWHM+1)
        x7=2*mul*x7/max(x7)-mul
        zz=x7* 2*SQRT(2*ALOG(2)) 
        dataX3=exp(-0.5*zz^2)      
 endelse
       
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Plot Mass Scale Calibration
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    WINDOW, 3, xsize=1250,ysize=700
    DEVICE, DECOMPOSED = 0 
    loadct,38
    !P.MULTI = [0,1,4] 
    !x.oMARGIN=[8,8]
    !Y.MARGIN = [3, 1]
 
    
    y=SumSpectrum/duration
    ymax=max(SumSpectrum/duration)
    y[where(y gt ymax/10)]=ymax/10
    
    
    plot, y, xtitle='TOF-bin',ytitle='signal [bin-cps]', xmargin=[15,6],background=-1,yrange=[0,ymax*0.13],thick=2, color=0, charsize=3
    loadct,31
    for ccc=0,23 do oplot,  [Top16[ccc,7],Top16[ccc,7]],  [ymax,ymax+ymax/15]/10 ,color=28, thick=3
    xyouts,max(size(SumSpectrum,/dimensions))/15,ymax*0.115, name, color=16, charsize=1.5,charthick=2      
 
    masses=m2t(peaklist[*,7],a,t0,ex,SampInt)
    mm=[19.0178, 37.0284, 29.9974, 31.9893, 45.9924]
    mm_1=[21.022, 39.0327,30.994, 33.9935, 47.9966]
    mm_2=[487, 242, 269, 242, 242]
    mm2=['m19: ','m37: ','m30: ','m32: ','m46: ']
    
    ;   calPairs=  [[21.022, 59.049], [30.994, 47.997], [21.022, 30.994], [33.994, 47.997], [21.022, 33.994]]
    ; NO  29.997   30.994
    ; NO2  45.992  47.9966
    ; O2  31.989 33.9935
    
    Psig=0
    Isig=0
    xyouts,max(size(SumSpectrum,/dimensions))/1.9,ymax*0.118, 'prim. sig [cps], local bg [cps], ratio sig/bg' , color=16, charsize=1.5,charthick=1  
    for r=0,4 do begin
            dm=1000*min(abs(masses-mm[r]))
            rr=floor(m2t([mm[r]-max([0.005, mm[r]/reso]),mm[r]+max([0.005, mm[r]/reso])],a,t0,ex,SampInt))
            sig=(corrtr(79)/corrtr(mm[r]))*total(SumSpectrum[rr[0]:rr[1]])/duration
            rr2=floor(m2t([mm_1[r]-max([0.005, mm_1[r]/reso]),mm_1[r]+max([0.005, mm_1[r]/reso])],a,t0,ex,SampInt))
            sig2=(corrtr(79)/corrtr(mm_1[r]))*mm_2[r]*total(SumSpectrum[rr2[0]:rr2[1]])/duration
            pp=floor((rr2[1]-rr2[0])/2)
            if(floor(1+(rr2[1]-rr2[0])/2) eq pp) then qq=pp-1 else qq=pp
            bk=total([SumSpectrum[-1-qq+rr2[0]:rr2[0]-1],SumSpectrum[1+rr2[1]:rr2[1]+1+pp]])/duration
            bk2=(corrtr(79)/corrtr(mm_1[r]))*mm_2[r]*total([SumSpectrum[-1-qq+rr2[0]:rr2[0]-1],SumSpectrum[1+rr2[1]:rr2[1]+1+pp]])/duration
            sig2=sig2-bk2
            SIGtoBK=sig2/bk2
           ;xyouts,max(size(SumSpectrum,/dimensions))/1.8,ymax*0.095-ymax*0.02*r, mm2[r]+string(sig,Format='(e11.1)')+string(sig2,Format='(e11.1)')+', dm='+string(dm,format='(F5.1)') , color=16, charsize=1.5,charthick=1  
            xyouts,max(size(SumSpectrum,/dimensions))/1.9,ymax*0.095-ymax*0.02*r, mm2[r]+string(sig2,Format='(e11.1)')+string(bk,Format='(e11.1)')+string(SIGtoBK,Format='(F8.2)'), color=16, charsize=1.5,charthick=1  
            Psig=[Psig,sig2]
    endfor
    Psig=Psig[1:*]
   if(where(Psig eq max(Psig)) lt 1.5 ) then mode='H3O+' else if(where(Psig eq max(Psig)) eq 3 ) then mode='O2+' else mode='NO+'
   xyouts,max(size(SumSpectrum,/dimensions))/2.8,ymax*0.115, mode, color=28, charsize=1.5,charthick=2      
   xyouts,max(size(SumSpectrum,/dimensions))/2.8,ymax*0.090, 'res='+string(floor(reso),format='(I4)'), color=28, charsize=1.5,charthick=2      
 xyouts,max(size(SumSpectrum,/dimensions))/2.8,ymax*0.065, 'base='+string(median(baselineSM),Format='(e10.1)'), color=28, charsize=1.5,charthick=2 
 
 
    tst=testSCALE(m2t(peaklist[*,7],a_raw,t0_raw,0.5, SampInt), lib2,instrument)
    print, 'rough cal:               a=', a_raw, '       t0=',t0_raw, '       ex=',0.5,'        scores:',tst.scorppm
    masses=tst.masses
    filter=where(abs(tst.deviation) lt 0.05)
      !x.oMARGIN=[54,36]
    !Y.MARGIN = [5, 1]
 
    if(max(filter) gt 0) then plot, masses[filter],1000*tst.deviation[filter], xtitle='m/z',ytitle='',yrange=[-60,60],background=239, color=16, psym = 4, xcharsize=3, ycharsize=3, xmargin=[15,6]
    if(max(filter) gt 0) then oplot, [0,800], [0,0], color=16
    filter=where(abs(tst.deviation) lt 0.002)
    if(max(filter) gt 0) then oplot, masses[filter],1000*tst.deviation[filter], psym = 4,thick=2, color=28
    if(max(filter) gt 0) then xyouts, 10,45, string(floor(tst.scorppm),format='(I4)')+' matches within 20 ppm, '+'mean deviation: '+string(20-100*(tst.scorppm-floor(tst.scorppm)),format='(F4.1)')+' ppm    a='+string(a_raw,format='(F7.1)')+' t0='+string(t0_raw,format='(F12.3)')+' ex=0.50000' ,color=28, charsize=1.2,charthick=2
    tst=testSCALE(m2t(peaklist[*,7],a_fine,t0_fine,ex_fine, SampInt), lib2,instrument)
    scorParVar=floor(tst.scorppm)
    print, 'fine cal:               a=', a, '       t0=',t0, '       ex=',ex,'        scores:',tst.scorppm
    masses=tst.masses
    filter=where(abs(tst.deviation) lt 0.05)
    if(max(filter) gt 0) then plot, xtitle='',ytitle='deviation to  library match [mDa]',yrange=[-60,60],background=239, color=16, masses[filter],1000*tst.deviation[filter], psym = 4, xcharsize=3, ycharsize=3, xmargin=[15,6]
    if(max(filter) gt 0) then oplot, [0,800], [0,0], color=16
    filter=where(abs(tst.deviation) lt 0.002)
    if(max(filter) gt 0) then oplot, masses[filter],1000*tst.deviation[filter], psym = 4, thick=2, color=28
    if(max(filter) gt 0) then xyouts, 10, 45, string(floor(tst.scorppm),format='(I4)')+' matches within 20 ppm, '+'mean deviation: '+string(20-100*(tst.scorppm-floor(tst.scorppm)),format='(F4.1)')+' ppm    a='+string(a_fine,format='(F7.1)')+' t0='+string(t0_fine,format='(F12.3)')+' ex='+string(ex_fine,format='(F7.5)'),color=28, charsize=1.2,charthick=2
    if(var_exists(a3) gt -0.5) then begin
            tst=testSCALE(m2t(peaklist[*,7],a3,t03,ex3, SampInt), lib2,instrument)
            scor3=floor(tst.scorppm)
            print, 'fine cal:               a=', a3, '       t0=',t03, '       ex=',ex3,'        scores:',tst.scorppm
            masses=tst.masses
            filter=where(abs(tst.deviation) lt 0.05)
            if(max(filter) gt 0) then plot, xtitle='',ytitle='',yrange=[-60,60],background=239, color=16, masses[filter],1000*tst.deviation[filter], psym = 4, xcharsize=3, ycharsize=3, xmargin=[15,6]
            if(max(filter) gt 0) then oplot, [0,800], [0,0], color=16
            filter=where(abs(tst.deviation) lt 0.002)
            if(max(filter) gt 0) then oplot, masses[filter],1000*tst.deviation[filter], psym = 4, thick=2, color=28
            if(max(filter) gt 0) then xyouts, 10, 45,string(floor(tst.scorppm),format='(I4)')+' matches within 20 ppm, '+'mean deviation: '+string(20-100*(tst.scorppm-floor(tst.scorppm)),format='(F4.1)')+' ppm    a='+string(a3,format='(F7.1)')+' t0='+string(t03,format='(F12.3)')+' ex='+string(ex3,format='(F7.5)'),color=28, charsize=1.2,charthick=2
    endif else begin
            plot, [0,1],[0,1],background=239, color=239
            xyouts, 0.2, 0.50,'No 3-point calibration (3rd point missing) ',color=28, charsize=2,charthick=2
            a3=-9999 & t03=-9999 & ex3=-9999 & scor3=-9999
    endelse
    for kk=0,max(size(name,/dimensions))-1 do  write_jpeg, destfold+'FileInfo/Time2MassPAR/'+'MassScale_'+name[kk]+'.jpg',tvrd(0,true=1), true=1
    !x.oMARGIN=[0,0]
   
 
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;prepare output:
    ;mass range, a, to, counts18,21,29,30,32,38,59, res18,21,29,30,32,38,59, error
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
    
    
    maxMass=m2t(max(x),a,t0,ex,Sampint)
    list=[18.0338,21.0221,29.0134,29.9974,31.9893,38.0326,59.0491,79.0542,93.0699,137.1325]
    masses=m2t(peaklist[*,7],a,t0,ex,SampInt)
    counts=0
    res=0
    rsrs=reso
    for k=0,9 do begin
            cnts=0
            rs=0
            if (min(abs(masses-list[k])) lt 0.003) then begin 
                    i1=sort(abs(masses-list[k])) & i1=i1[0] 
                    tL=m2t(m2t(peaklist[i1,7],a,t0,ex,SampInt)*(1-1/(rsrs*2*SQRT(2*ALOG(2)))),a,t0,ex,SampInt)
                    tH=m2t(m2t(peaklist[i1,7],a,t0,ex,SampInt)*(1+1/(rsrs*2*SQRT(2*ALOG(2)))),a,t0,ex,SampInt)
                    subdata=SumSpectrum[tL:tH]     
                    subx=x[tL:tH]      
                    fitLow=GAUSSFIT(subx,subdata,coeff, NTERMS=3)
                    cnts=coeff[0]*coeff[2]*sqrt(2*!pi)
                    ms=coeff[1]
                    msL=coeff[1]-SQRT(2*ALOG(2))*coeff[2]
                    msH=coeff[1]+SQRT(2*ALOG(2))*coeff[2]
                    rs=((ms-t0)/a)^2/(((msH-t0)/a)^2-((msL-t0)/a)^2)
            endif else begin
                    cnts=-9999
                    rs=-9999
            endelse
            counts=[counts,cnts]
            res=[res,rs]
    endfor
    
    
    output=[maxMass,a,t0,counts[1:10],res[1:10]]
    massnames=['m18.0338','m21.0221','m29.0134','m29.9974','m31.9893','m38.0326','m59.0491','m79.0542','m93.0699','m137.1325']
    peaklist[*,0]=masses
    peaklist[*,1]=m2t(peaklist[*,1],a,t0,ex,SampInt)     
    peaklist[*,2]=m2t(peaklist[*,2],a,t0,ex,SampInt)    
    peaklist[*,3]=masses/(peaklist[*,2]-peaklist[*,1])
    peaklistnames=['PeakMax[Da]','StartPeak[Da]','EndPeak[Da]','PeakBroadness','SlopeMax','SlopeMin','PeakHight[counts]','PeakMax[time]','BaseHight[counts]']
    s1=CREATE_STRUCT('MaxMass',maxMass,'a', a,'t0',t0,'ex',ex,'scorParVar',scorParVar,'a3', a3,'t03',t03,'ex3',ex3,'scor3',scor3,'massnames',massnames,$
    'counts', counts[1:10],'res',res[1:10],'masslistnames',peaklistnames,'masslist',peaklist, $
    'baseline', baselineSM, 'PeakShape',[[x7],[dataX3]],'resolution',reso, 'mode', mode,'ResolutionVsMass',[[MBresolution],[MBresolutionM]])
endif else begin ;this is done if calcrude failed
  s1=CREATE_STRUCT('MaxMass',-9999,'a', -9999,'t0',-9999,'ex',-9999,'scorParVar',-9999,'a3', -9999,'t03',-9999,'ex3',-9999,'scor3',-9999,'massnames',-9999,$
    'counts', -9999,'res',-9999,'masslistnames',-9999,'masslist',-9999, $
    'baseline',-9999, 'PeakShape',-9999,'resolution',-9999, 'mode',-9999)
endelse   
    Return, s1
end


function PS2,x,y

sepp=6 ; limit to seperate independent peaks, unit: bins of bin mass scale,i.e. 48 ppm above 125 Da
ysm=smooth(y,5)
dysm=deriv(x,ysm)  
length=size(y,/dimensions)
index=lindgen(length)
PEAKS=max([[ysm],[0,ysm[0:length-2]],[ysm[1:length-1],0],[0,0,ysm[0:length-3]],[ysm[2:length-1],0,0]],dimension=2); PEAKS[i] = max of ysm[i-2:i+2] for all elements
PEAKS[where(dysm lt 0)]=0
PEAKS[where([dysm[1:length-1],0] gt 0)]=0
PEAKS[where(PEAKS lt max(ysm)/20)]=0
PEAKS[where(PEAKS lt 0.55)]=0
PEAKS=index[where(PEAKS gt 0)]   
length2=max(size(PEAKS,/dimensions)) ; number of peaks  

print, 'HHGHGH',length2
xpeaks=0
sgma=0
ymax=0
ymax2=0
aerM1s=0
aerM2s=0
aerP1s=0
aerP2s=0
notres=1
for i=0,length2-1 do begin
 
        indis=lindgen(11)+PEAKS[i]-5
        xi=x[indis]
        r=gaussfit(xi,y[indis]+0.0001, coeff, chisq=fff)
        if(max(y[indis])le 10 ) then   r=gaussfit(xi,y[indis],coeff, chisq=fff,nterms=3)
        if(coeff[0] lt 0 or coeff[0] gt max(y[indis])*3 or 1e6*abs(coeff[2])/mean(xi) lt 5 or max(y[indis])le 5 ) then   r=gaussfit(xi,ysm[indis],coeff, chisq=fff, nterms=3)
       ; if(coeff[0] lt 0 or coeff[0] gt max(y[indis])*3 or coeff[2] lt (5/2*SQRT(2*ALOG(2)))*abs(coeff[2])/mean(xi) or max(y[indis])le 5 ) then   r=gaussfit(xi,ysm[indis],coeff, chisq=fff, nterms=3)
    if (coeff[1] gt 125) then seplim=48 else seplim=float(sepp)*1000/coeff[1]
        xpeaks=[xpeaks,coeff[1]]
        ymax=[ymax,coeff[0]]
        flt=where(abs(x-coeff[1]) eq min(abs(x-coeff[1])))
        ymax2=[ymax2,ysm[flt[0]]]
        sgma=[sgma,coeff[2]]
        flt2=where(x gt coeff[1]-2*coeff[2] and x le coeff[1]-coeff[2]) & if(max(flt2) lt -0.5) then flt2=flt-1
        aerM2s=[aerM2s,total(y[flt2])]
        flt2=where(x gt coeff[1]-coeff[2] and x le coeff[1]) & if(max(flt2) lt -0.5) then flt2=flt
        aerM1s=[aerM1s,total(y[flt2])]
        flt2=where(x gt coeff[1] and x le coeff[1]+coeff[2]) & if(max(flt2) lt -0.5) then flt2=flt
        aerP1s=[aerP1s,total(y[flt2])]
        flt2=where(x gt coeff[1]+coeff[2] and x le coeff[1]+2*coeff[2]) & if(max(flt2) lt -0.5) then flt2=flt+1
        aerP2s=[aerP1s,total(y[flt2])]
        check=0
        
        if(coeff[0] lt 0) then notres=[notres,1] else if(coeff[0] gt 2.5*max(y[indis])) then notres=[notres,1] else $
              if(coeff[1] lt min(xi) or coeff[1] gt max(xi)) then notres=[notres,1] else $
              if(coeff[2] lt 0) then notres=[notres,1] else $
              if(min(abs(xpeaks[0:i]-coeff[1])/coeff[1]) lt seplim*1e-6) then check=1 else $
               notres=[notres,0]
       
        if(check eq 1) then begin
           iii=where(abs((xpeaks[0:i]-coeff[1])/coeff[1]) lt seplim*1e-6) 
           if(min(notres[iii]) eq 1) then notres=[notres,0] else begin
               iv=iii[where(notres[iii] eq 0)]
                if (max(ymax2[iv]) gt ysm[flt[0]]) then notres=[notres,1] 
                if (max(ymax2[iv]) le ysm[flt[0]]) then begin
                    notres[iv]=1
                    notres=[notres,0]
                endif    
           endelse
       endif
      ;  if( x[peaks[i]] gt 180 and x[peaks[i]] lt 190 ) then  print, fff, coeff[0],coeff[1],coeff[2],x[peaks[i]],max(y[indis])
    
endfor   
;notres[where(ysm[PEAKS] eq 0)
   xpeaks=xpeaks[where(notres eq 0)]
   sgma=sgma[where(notres eq 0)]
   ymax=ymax[where(notres eq 0)]
   ymax2=ymax2[where(notres eq 0)]
   aerM1s=aerM1s[where(notres eq 0)]
   aerM2s=aerM2s[where(notres eq 0)]
   aerP1s=aerM1s[where(notres eq 0)]
   aerP2s=aerM2s[where(notres eq 0)]
   PEAKS=[0,PEAKS]
   PEAKS=PEAKS[where(notres eq 0)]
   area=ymax*sgma
names=['xMaxDeriv','xMaxGauss','sigma','yMaxFit','Area','ysm@xpeaks','aer-1*sigma','aer-2*sigma','aer+1*sigma','aer+2*sigma']


PeakList=[[x[PEAKS]],[xpeaks],[sgma],[ymax],[area],[ymax2],[aerM1s],[aerM2s],[aerP1s],[aerP2s]]
;PeakList=[[x[PEAKS]],[xpeaks[1:*]],[fwhm[1:*]],[ymax[1:*]],[area[1:*]]]
s1=CREATE_STRUCT('names',names,'data',PeakList)

  
Return, s1
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;                   Auxiliary routines
;;;;;;;
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO ____________aux_routines
 end

function CompoundClass, formula

if(strpos(formula,'13CC') gt -0.5) then begin
numb1=strmid(formula,strpos(formula,'13CC')+4,1)
numb2=strmid(formula,strpos(formula,'13CC')+5,1)
if(strpos('1234567890',numb1) gt -0.5) then if (strpos('1234567890',numb2) gt -0.5) then C=long(numb1)*10+long(numb2) +1
if(strpos('1234567890',numb1) gt -0.5) then if (strpos('1234567890',numb2) lt -0.5)then C=long(numb1) +1
if(strpos('1234567890',numb1) lt -0.5) then C=2
endif

if(strpos(formula,'13CC') lt -0.5 and strpos(formula,'C') gt -0.5) then begin
numb1=strmid(formula,strpos(formula,'C')+1,1)
numb2=strmid(formula,strpos(formula,'C')+2,1)
if(strpos('1234567890',numb1) gt -0.5) then if (strpos('1234567890',numb2) gt -0.5) then C=long(numb1)*10+long(numb2) 
if(strpos('1234567890',numb1) gt -0.5) then if (strpos('1234567890',numb2) lt -0.5)then C=long(numb1) 
if(strpos('1234567890',numb1) lt -0.5) then C=1
endif  

if(strpos(formula,'C') lt -0.5) then C=0

if(strpos(formula,'SO4') gt -0.5) then S=1 else S=0

if(strpos(formula,'N') gt -0.5) then begin
numb=strmid(formula,strpos(formula,'N')+1,1)
if(strpos('1234567890',numb) gt -0.5) then N=long(numb) else N=1
endif  else N=0

if(strpos(formula,'O') gt -0.5) then begin
numb=strmid(formula,strpos(formula,'O')+1,1)
if(strpos('1234567890',numb) gt -0.5) then O=long(numb) else O=1
endif  else O=0

H=0
if(strpos(formula,'H') gt -0.5) then begin
numb1=strmid(formula,strpos(formula,'H')+1,1)
numb2=strmid(formula,strpos(formula,'H')+2,1)
if(strpos('1234567890',numb1) gt -0.5) then if (strpos('1234567890',numb2) gt -0.5) then H=long(numb1)*10+long(numb2) 
if(strpos('1234567890',numb1) gt -0.5) then if (strpos('1234567890',numb2) lt -0.5)then H=long(numb1)
if(strpos('1234567890',numb1) lt -0.5) then if(strpos('+',numb1) gt -0.5) then H=0 else H=1
endif

type=''

if (C eq 0) then type=[type,'inorg']
if (C ge 1) then type=[type,'org_']
if (C ge 1 and O eq 0 and N eq 0 and S eq 0) then type=[type,'HC_']
if (C ge 1  and S ge 1) then type=[type,'orgSO4']

if (C ge 1 and S eq 0  and N ge 1) then type=[type,'orgN_']
if (C ge 1 and S eq 0  and N eq 1) then type=[type,'orgN1']
if (C ge 1 and S eq 0  and N eq 2) then type=[type,'orgN2']
if (C ge 1 and S eq 0   and O eq 0 and N ge 1) then type=[type,'NHCO0']
if (C ge 1 and S eq 0   and O eq 1 and N ge 1) then type=[type,'NHCO1']
if (C ge 1 and S eq 0   and O eq 2 and N ge 1) then type=[type,'NHCO2']
if (C ge 1 and S eq 0   and O eq 3 and N ge 1) then type=[type,'NHCO3']
if (C ge 1 and S eq 0   and O eq 4 and N ge 1) then type=[type,'NHCO4']
if (C ge 1 and S eq 0   and O eq 5 and N ge 1) then type=[type,'NHCO5']
if (C ge 1 and S eq 0   and O eq 6 and N ge 1) then type=[type,'NHCO6']
if (C ge 1 and S eq 0   and O eq 7 and N ge 1) then type=[type,'NHCO7']
if (C ge 1 and S eq 0   and O eq 8 and N ge 1) then type=[type,'NHCO8']
if (C ge 1 and S eq 0  and N eq 0) then type=[type,'orgNoN']
if (C ge 1 and S eq 0 and O ge 1  and N eq 0) then type=[type,'HCO_']
if (C ge 1 and S eq 0 and O eq 1 and N eq 0) then type=[type,'HCO1']
if (C ge 1 and S eq 0 and O eq 2 and N eq 0) then type=[type,'HCO2']
if (C ge 1 and S eq 0 and O eq 3 and N eq 0) then type=[type,'HCO3']
if (C ge 1 and S eq 0 and O eq 4 and N eq 0) then type=[type,'HCO4']
if (C ge 1 and S eq 0 and O eq 5 and N eq 0) then type=[type,'HCO5']
if (C ge 1 and S eq 0 and O eq 6 and N eq 0) then type=[type,'HCO6']
if (C ge 1 and S eq 0 and O eq 7 and N eq 0) then type=[type,'HCO7']
if (C ge 1 and S eq 0 and O eq 8 and N eq 0) then type=[type,'HCO8']

if(N gt 0) then unsat='unsat='+string((2.0+2*float(C)+float(N)-float(H))/2,format='(F4.1)')+'N' else unsat='unsat='+string((2.0+2*float(C)+float(N)-float(H))/2,format='(F4.1)')+'C'
type=[type,unsat]
;if(N gt 0) then OSC='OSC=-9999.00' else OSC='OSC='+string(2*float(O)/float(C)-float(H)/float(C),format='(F8.5)')
;if(N gt 100) then OSC='OSC=-9999.00' else OSC='OSC='+string(2*float(O)/(float(N)+float(C))-float(H)/(float(N)+float(C)),format='(F8.5)')
if(N gt 100) then OSC='OSC=-9999.00' else OSC='OSC='+string(2*float(O)/float(C)-getpar('N_ox_st')*float(N)/float(C)-float(H)/float(C),format='(F8.5)')
type=[type,OSC]
nC='nC='+string(C,format='(I2)')
type=[type,nC]
nH='nH='+string(H,format='(I2)')
type=[type,nH]
nO='nO='+string(O,format='(I2)')
type=[type,nO]
nN='nN='+string(N,format='(I2)')
type=[type,nN]
return, type[1:*]
end



function d, array
return, size(array,/dimensions)
end



pro DispCT, index
!P.MULTI = [0,1,6]   
DEVICE, DECOMPOSED = 0 
loadct,index
for k=0,5 do begin
        plot, [-25,25]+k*50,[0,0],yrange=[0,1],xrange=[-25,25]+k*50,background=-1, color=0,charsize=2
        for i=-25,25 do oplot, [i,i]+k*50,[0,1],thick=15,color=i+k*50
endfor
end
function drucksieb, string,fontsize
; subroutine to write numbers into images
  komma=[1,2,4,5]
  eins=[3,4,8,9,13,14,18,19,23,24,26,27,28,29,33,34,39]
  zwei=[1,2,3,4,5,7,8,14,15,21,22,28,34,35,37,38,40,41,44,45,46]
  drei=[2,3,4,7,8,10,11,16,17,21,22,27,28,34,35,37,38,40,41,44,45,46]
  vier=[4,10,13,14,15,16,17,19,22,26,28,32,34,39,40,45,46]
  fuenf=[2,3,4,7,8,10,11,16,17,22,23,25,26,27,28,31,32,37,38,43,44,45,46,47]
  sechs=[3,4,8,10,11,13,14,16,17,19,20,22,23,25,26,27,28,31,32,38,40,45,46]
  sieben=[2,3,8,9,15,21,27,28,33,34,40,41,43,44,45,46,47]
  acht=[2,3,4,7,8,10,11,13,14,16,17,20,21,22,26,27,28,31,32,34,35,37,38,40,41,44,45,46]
  neun=[2,3,8,10,16,17,20,21,22,23,25,26,28,29,31,32,34,35,37,38,40,44,45]
  null=[3,8,10,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,38,40,45]
  
  length=strlen(string)
  siebmat=intarr(1,8)
  for i=0,length-1 do begin
      substri=strmid(string,i,1)
      submat=intarr(6,8)
      if substri EQ '.' then begin
          submat=intarr(3,8) 
          submat[komma]=-2
      endif
      if substri EQ '1' then begin 
          submat=intarr(5,8)
          submat[eins]=-2
      endif   
      
      if substri EQ '2' then submat[zwei]=-2  
      if substri EQ '3' then submat[drei]=-2  
      if substri EQ '4' then submat[vier]=-2 
      if substri EQ '5' then submat[fuenf]=-2  
      if substri EQ '6' then submat[sechs]=-2
      if substri EQ '7' then submat[sieben]=-2
      if substri EQ '8' then submat[acht]=-2  
      if substri EQ '9' then submat[neun]=-2 
      if substri EQ '0' then submat[null]=-2
      siebmat=[siebmat,submat]
  endfor
  dim=size(siebmat,/dimensions)
  expand, siebmat,dim[0]*fontsize,dim[1]*fontsize,siebmatrix
  if(min(siebmatrix) gt -0.5) then siebmatrix[0:4]=-2
  siebmatrix[where(siebmatrix LT -0.5)]=-9999
  Return, siebmatrix
end


function FileList, event, Path
     compile_opt idl2         
; retrieve filelist
  Err=''
  locind=0
  addind=0
  location='/FullSpectra/TofData'
  oldfile='-9999'
  mist=systime(1)
  Recur_Pattern = '*.h5'
  Files = FILE_SEARCH(Path,Recur_Pattern) 
  ; Remove the '_PTR' files from the filename list
  n = N_ELEMENTS(Files) 
  S=STRPOS(Files[0], '_PTR');
  if (n gt 1) then FOR i=1,n-1 DO  S = [S,STRPOS(Files[i], '_PTR')]
  Files = Files[where(S EQ -1)]
  ; Remove non-HDF5 files from list
  n = N_ELEMENTS(Files) 
  errFiles=-1
jj=0
  
  location='/FullSpectra/TofData'
  CATCH, Error_status 
  IF Error_status NE 0 THEN BEGIN 
          if(strpos(location,'/SPECdata/Intensities') eq 0) then  begin 
                  location='/FullSpectra/TofData'
                  errFiles=[errFiles,jj] 
          endif else begin
                  location='/SPECdata/Intensities'
                  errFiles=[errFiles,jj-1] 
          endelse
          print, ''
  ENDIF 
  
  if(max(errFiles) eq jj) then Err=[Err,'','Discard: '+Files[max(errFiles)]]
  FOR jj=max(errFiles)+1,n-1 DO if (min(abs(errFiles-jj))gt 0) then begin 
          file_id = H5f_OPEN(Files[jj]) 
          dataset_id1 = H5D_OPEN(file_id,  location) 
          dataspace_id1 = H5D_GET_SPACE(dataset_id1) 
          Dims=H5S_GET_SIMPLE_EXTENT_DIMS(Dataspace_id1) 
          Start =10000
          Width = 2
          if (max(size(Dims,/dimensions)) ge 2) then begin
                  ddd=lonarr(size(Dims,/dimensions)-1)
                  eee=ddd
                  for ii=1,max(size(Dims,/dimensions))-1 do eee[ii-1]=Dims[ii]
                  start2 = [Start, ddd] 
                  count = [Width,ddd+1] 
          endif else begin
                  start2 = [Start] 
                  count = [Width] 
          endelse
          H5S_SELECT_HYPERSLAB, dataspace_id1, start2, count,  /RESET 
          memory_space_id1 = H5S_CREATE_SIMPLE(count) 
          ptrData=H5D_READ(dataset_id1, FILE_SPACE=dataspace_id1, MEMORY_SPACE=memory_space_id1) ; Data
          H5S_CLOSE, memory_space_id1
          H5d_CLOSE, dataset_id1
          H5S_CLOSE, dataspace_id1 
          H5f_CLOSE,  file_id
          if (var_exists(ind) gt 0) then ind=[ind,jj] else ind=jj
          ;  print, jj
          info=['Checking files:',string(jj+1,format='(I5)')+'   of '+string(n,format='(I5)')+' files']
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= [info, Err]
  endif
  CATCH, /CANCEL 
  
  
  Files=Files[ind] 
  n = N_ELEMENTS(Files) 
 
CreationTime='nix'
StartTimes=0
StartTimesStr='nix'
      
FOR i=0,n-1 DO  begin

        fid=H5f_OPEN(Files[i]) 
        n_attr=H5A_GET_NUM_ATTRS(fid)
        mistt='nix'
        for j=0,n_attr-1 do if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(fid,j)),'HDF5 File Creation Time') gt -0.5) then mistt=H5A_read( H5A_OPEN_IDX(fid,j))
        if(strpos(mistt ,'nix') eq 0) then for j=0,n_attr-1 do if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(fid,j)),'FileCreatedTimeSTR') gt -0.5) then mistt=H5A_read( H5A_OPEN_IDX(fid,j))
   
      CreationTime=[CreationTime,mistt]
      tt09=t09(mistt)
      StartTimes=[StartTimes,tt09]
      StartTimesStr=[StartTimesStr,t09str(tt09)]
      info2=['Reading start times:',string(i+1,format='(I5)')+'   of '+string(n,format='(I5)')+' files']
      WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= [info,info2,Err]
   
endfor

CreationTime=CreationTime[1:*]
StartTimes=StartTimes[1:*]
StartTimesStr=StartTimesStr[1:*]

files=files[sort(StartTimes)]    
StartTimesStr=StartTimesStr[sort(StartTimes)]
StartTimes=StartTimes[sort(StartTimes)]

s1=CREATE_STRUCT('path',files,'t09',starttimes,'disp',StartTimesStr)
  
  
   Return, s1
end





function FileLst, Path
; retrieve filelist
  Recur_Pattern = '*UL.fdt'
  Files = FILE_SEARCH(Path,Recur_Pattern) 
  files=files[where(strpos(Files,'ppb20') ne -1)]
  files=files[where(strpos(Files,'corrppb20') eq -1)]
  
  n = N_ELEMENTS(Files) 
  StartTimes = starttimes(files,'ppb20')
  files=files[sort(StartTimes)]    
  StartTimes=StartTimes[sort(StartTimes)]
  mist=SIZE(Files, /DIMENSIONS)
  if(max(mist) gt 2)then $
        mist3= transpose(string(indgen(size(files,/dimensions)),format='(i4)')+' - ')+ transpose(strmid(Files,transpose(strpos(Files,'ppb20')+3),20)) $
        else mist3= string(indgen(size(files,/dimensions)),format='(i4)')+' - '+ strmid(Files,strpos(Files,'ppb20')+3,20) 
  
  filter= where(strpos(files, 'UL.fdt') gt -0.5)
  files=files[filter]
  mist3=mist3[filter]
  s1=CREATE_STRUCT('path',files,'disp',mist3, 'startTimes',startTimes)
  Return, s1
end


pro fitfunc, x, A,F, pder
  z=(x-A[1])/A[2]
  exz=exp(-z*z/2)
  F=A[0]*exz ;+ A[3] + A[4]*x
  
  ;If the procedure is called with four parameters, calculate the 
;partial derivatives. 
  IF N_PARAMS() GE 4 THEN $ 
  ;  pder = transpose([transpose(exz), transpose(A[0] * (X-A[1]) * exz / (A[2]^2)), transpose(A[0] * (X-A[1])^2 * exz / (A[2]^3)),transpose(replicate(1.0, N_ELEMENTS(X))),transpose(x)]) 
    pder = transpose([transpose(exz), transpose(A[0] * (X-A[1]) * exz / (A[2]^2)), transpose(A[0] * (X-A[1])^2 * exz / (A[2]^3))]) 
  
end  


function formula, lib
length=size(lib,/dimensions) & if(size(length,/dimensions) gt 1) then length=max(length[1])else length=1
C12=strarr(length)
C13=strarr(length)
H1=strarr(length)
O16=strarr(length)
O17=strarr(length)
O18=strarr(length)
N14=strarr(length)
N15=strarr(length)
Hpos=strarr(length)
eneg=strarr(length)
Cl35=strarr(length)
Cl37=strarr(length)
F=strarr(length)
S32O4=strarr(length)
S34O4=strarr(length)

len=size(lib,/dimensions) & len=max(len[0])

FIL=where(lib[1,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then C12[where(lib[1,indgen(length)] gt 0)]='C'
filter=where(lib[1,indgen(length)] gt 9.5) & if(max(filter) gt -0.5) then C12[filter]=C12[filter]+string(lib[1,filter],format='(I2)') 
filter=where(lib[1,indgen(length)] gt 1.5 and lib[1,indgen(length)] lt 9.5) & if(max(filter) gt -0.5) then C12[filter]=C12[filter]+string(lib[1,filter],format='(I1)')

FIL=where(lib[2,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then C13[where(lib[2,indgen(length)] gt 0)]='13C'

FIL=where(lib[3,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then H1[where(lib[3,indgen(length)] gt 0)]='H'
filter=where(lib[3,indgen(length)] gt 9.5) & if(max(filter) gt -0.5) then  H1[filter]=H1[filter]+string(lib[3,filter],format='(I2)') 
filter=where(lib[3,indgen(length)] gt 1.5 and lib[3,indgen(length)] lt 9.5) & if(max(filter) gt -0.5) then H1[filter]=H1[filter]+string(lib[3,filter],format='(I1)')

FIL=where(lib[4,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then O16[where(lib[4,indgen(length)] gt 0)]='O'
filter=where(lib[4,indgen(length)] gt 9.5) & if(max(filter) gt -0.5) then O16[filter]=O16[filter]+string(lib[4,filter],format='(I2)')
filter=where(lib[4,indgen(length)] gt 1.5 and lib[4,indgen(length)] lt 9.5) & if(max(filter) gt -0.5) then O16[filter]=O16[filter]+string(lib[4,filter],format='(I1)')

FIL=where(lib[5,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then O17[where(lib[5,indgen(length)] gt 0)]='17O'
FIL=where(lib[6,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then O18[where(lib[6,indgen(length)] gt 0)]='18O'


FIL=where(lib[7,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then N14[where(lib[7,indgen(length)] gt 0)]='N'
filter=where(lib[7,indgen(length)] gt 9.5) & if(max(filter) gt -0.5) then N14[filter]=N14[filter]+string(lib[7,filter],format='(I2)')
filter=where(lib[7,indgen(length)] gt 1.5 and lib[7,indgen(length)] lt 9.5) & if(max(filter) gt -0.5) then N14[filter]=N14[filter]+string(lib[7,filter],format='(I1)')

FIL=where(lib[8,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then N15[where(lib[8,indgen(length)] gt 0)]='15N'

if(len gt 11) then begin
FIL=where(lib[11,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then Cl35[where(lib[11,indgen(length)] gt 0)]='35Cl'
filter=where(lib[11,indgen(length)] gt 1.5) & if(max(filter) gt -0.5) then Cl35[filter]=Cl35[filter]+string(lib[11,filter],format='(I1)')
FIL=where(lib[12,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then Cl37[where(lib[12,indgen(length)] gt 0)]='37Cl'
filter=where(lib[12,indgen(length)] gt 1.5) & if(max(filter) gt -0.5) then Cl37[filter]=Cl37[filter]+string(lib[12,filter],format='(I1)')

FIL=where(lib[13,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then F[where(lib[13,indgen(length)] gt 0)]='F'
filter=where(lib[13,indgen(length)] gt 1.5) & if(max(filter) gt -0.5) then F[filter]=F[filter]+string(lib[13,filter],format='(I1)')

FIL=where(lib[14,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then S32O4[where(lib[14,indgen(length)] gt 0)]='SO4'
filter=where(lib[14,indgen(length)] gt 1.5) & if(max(filter) gt -0.5) then S32O4[filter]=S32O4[filter]+string(lib[14,filter],format='(I1)')
FIL=where(lib[15,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then S34O4[where(lib[15,indgen(length)] gt 0)]='34SO4'
filter=where(lib[15,indgen(length)] gt 1.5) & if(max(filter) gt -0.5) then S34O4[filter]=S34O4[filter]+string(lib[15,filter],format='(I1)')

endif

FIL=where(lib[9,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then Hpos[where(lib[9,indgen(length)] gt 0)]='H+'

FIL=where(lib[10,indgen(length)] gt 0) &  if(max(FIL) gt -0.5) then eneg[where(lib[10,indgen(length)] gt 0)]='+'




form=C13+C12+H1+O18+O17+O16+N15+N14+Cl35+Cl37+F+S32O4+S34O4+Hpos+eneg
return, transpose(form)
end


function GetEngData, event, File
  compile_opt idl2
 
info=strarr(7)
info[0]='H5 OPEN:'
;a=H5_BROWSER(File)
File2=strmid(file,0,strpos(file,'.h5'))+'_PTR.h5'
misst=systime(1)
ErrCode=0
s1=CREATE_STRUCT('duration',-9999)

;open file
file_id = H5f_OPEN(File) 
print, 'H5 FILE OPEN:  '+File
 
 
 
 
; open SumSpec and Bufftimes
  T1000=-1
  CATCH, Error_status 
  IF Error_status NE 0 THEN  T1000=1
  if (T1000 eq -1) then begin
          BufTimes_id = H5D_OPEN(file_id, '/TimingData/BufTimes')
          info[1]='/TimingData/BufTimes'
          SumSpec_id = H5D_OPEN(file_id, '/FullSpectra/SumSpectrum')
          info[2]='/FullSpectra/SumSpectrum'
          instrument='TOF8000'
  endif
  if (T1000 eq 1) then begin
          T1000=-1
          BufTimes_id = H5D_OPEN(file_id, '/SPECdata/Times')
          info[1]='/SPECdata/Times'
          SumSpec_id = H5D_OPEN(file_id, '/SPECdata/AverageSpec')
          info[2]='/SPECdata/AverageSpec'
          instrument='TOF1000'
   endif
  CATCH, /CANCEL 
  
  
  
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE= info
  WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE=info
 
 



;open Engineering data

CATCH, Error_status 
IF Error_status NE 0 THEN BEGIN 
 ;     PRINT, 'Error index: ', Error_status 
 ;     PRINT, 'Error message: ', !ERROR_STATE.MSG 
      if(strpos(string(!ERROR_STATE.MSG ),'AddTraces/PTR-Reaction/Info') ge 0) then  ErrCode=[ErrCode,7] else if(strpos(string(!ERROR_STATE.MSG ),'AddTraces/PTR-Reaction/TwInfo') ge 0) then  ErrCode=[ErrCode,4] else if(strpos(string(!ERROR_STATE.MSG ),'PTR-Reaction/TwInfo') ge 0) then  ErrCode=[ErrCode,1] else if(strpos(string(!ERROR_STATE.MSG ),'PTR-Reaction/Info') ge 0) then  ErrCode=[ErrCode,1]       
      if(strpos(string(!ERROR_STATE.MSG ),'AddTraces/PTR-Instrument/Info') ge 0) then  ErrCode=[ErrCode,8] else if(strpos(string(!ERROR_STATE.MSG ),'AddTraces/PTR-Instrument/TwInfo') ge 0) then  ErrCode=[ErrCode,5] else if(strpos(string(!ERROR_STATE.MSG ),'PTR-Instrument/TwInfo') ge 0) then  ErrCode=[ErrCode,2] else if(strpos(string(!ERROR_STATE.MSG ),'PTR-Instrument/Info') ge 0) then  ErrCode=[ErrCode,2]
      if(strpos(string(!ERROR_STATE.MSG ),'AddTraces/PTR-Misc/Info') ge 0) then  ErrCode=[ErrCode,9]  else if(strpos(string(!ERROR_STATE.MSG ),'AddTraces/PTR-Misc/TwInfo') ge 0) then  ErrCode=[ErrCode,6]  else if(strpos(string(!ERROR_STATE.MSG ),'PTR-Misc/TwInfo') ge 0) then  ErrCode=[ErrCode,3] else if(strpos(string(!ERROR_STATE.MSG ),'PTR-Misc/Info') ge 0) then  ErrCode=[ErrCode,3]
      if(strpos(string(!ERROR_STATE.MSG ),'AddTraces/OptionBOX/Info') ge 0) then  ErrCode=[ErrCode,11]  else if(strpos(string(!ERROR_STATE.MSG ),'AddTraces/OptionBOX/TwInfo') ge 0) then  ErrCode=[ErrCode,10]  
      if(strpos(string(!ERROR_STATE.MSG ),File2) ge 0) then  ErrCode=[ErrCode,20]  
ENDIF 

if(ErrCode[0] lt 100) then begin
        if(max(where(ErrCode eq 20)) lt -0.5) then begin 
                file_id2 = H5f_OPEN(File2)    
        endif        
        if(var_exists(file_id2) gt -0.5) then begin       
                if(max(where(ErrCode eq 1)) lt -0.5) then begin 
                        NamesPTR3_id = H5D_OPEN(file_id2, '/PTR-Reaction/Info') 
                        DataPTR3_id = H5D_OPEN(file_id2, '/PTR-Reaction/Data')
                endif
                if(max(where(ErrCode eq 2)) lt -0.5) then begin
                        NamesPTR1_id = H5D_OPEN(file_id2, '/PTR-Instrument/Info') 
                        DataPTR1_id = H5D_OPEN(file_id2, '/PTR-Instrument/Data') 
                endif
                if(max(where(ErrCode eq 3)) lt -0.5) then begin 
                        NamesPTR2_id = H5D_OPEN(file_id2, '/PTR-Misc/Info') 
                        DataPTR2_id = H5D_OPEN(file_id2, '/PTR-Misc/Data')
                endif
        endif else begin
               if(max(where(ErrCode eq 1)) lt -0.5) then begin 
                        NamesPTR3_id = H5D_OPEN(file_id, '/PTR-Reaction/TwInfo') 
                        DataPTR3_id = H5D_OPEN(file_id, '/PTR-Reaction/TwData')
                endif
                if(max(where(ErrCode eq 2)) lt -0.5) then begin
                        NamesPTR1_id = H5D_OPEN(file_id, '/PTR-Instrument/TwInfo') 
                        DataPTR1_id = H5D_OPEN(file_id, '/PTR-Instrument/TwData') 
                endif
                if(max(where(ErrCode eq 3)) lt -0.5) then begin 
                        NamesPTR2_id = H5D_OPEN(file_id, '/PTR-Misc/TwInfo') 
                        DataPTR2_id = H5D_OPEN(file_id, '/PTR-Misc/TwData')
                endif   
                if(max(where(ErrCode eq 4)) lt -0.5) then begin 
                        NamesPTR3_id = H5D_OPEN(file_id, '/AddTraces/PTR-Reaction/TwInfo') 
                        DataPTR3_id = H5D_OPEN(file_id, '/AddTraces/PTR-Reaction/TwData')
                endif
                if(max(where(ErrCode eq 5)) lt -0.5) then begin
                        NamesPTR1_id = H5D_OPEN(file_id, '/AddTraces/PTR-Instrument/TwInfo') 
                        DataPTR1_id = H5D_OPEN(file_id, '/AddTraces/PTR-Instrument/TwData') 
                endif
                if(max(where(ErrCode eq 6)) lt -0.5) then begin 
                        NamesPTR2_id = H5D_OPEN(file_id, '/AddTraces/PTR-Misc/TwInfo') 
                        DataPTR2_id = H5D_OPEN(file_id, '/AddTraces/PTR-Misc/TwData')
                endif
                if(max(where(ErrCode eq 7)) lt -0.5) then begin 
                        NamesPTR3_id = H5D_OPEN(file_id, '/AddTraces/PTR-Reaction/Info') 
                        DataPTR3_id = H5D_OPEN(file_id, '/AddTraces/PTR-Reaction/Data')
                endif
                if(max(where(ErrCode eq 8)) lt -0.5) then begin
                        NamesPTR1_id = H5D_OPEN(file_id, '/AddTraces/PTR-Instrument/Info') 
                        DataPTR1_id = H5D_OPEN(file_id, '/AddTraces/PTR-Instrument/Data') 
                endif
                if(max(where(ErrCode eq 9)) lt -0.5) then begin 
                        NamesPTR2_id = H5D_OPEN(file_id, '/AddTraces/PTR-Misc/Info') 
                        DataPTR2_id = H5D_OPEN(file_id, '/AddTraces/PTR-Misc/Data')
                endif
                if(max(where(ErrCode eq 10)) lt -0.5) then begin 
                        NamesPTR2_id = H5D_OPEN(file_id, '/AddTraces/OptionBOX/TwInfo') 
                        DataPTR2_id = H5D_OPEN(file_id, '/AddTraces/OptionBOX/TwData')
                endif
               if(max(where(ErrCode eq 11)) lt -0.5) then begin 
                        NamesPTR2_id = H5D_OPEN(file_id, '/AddTraces/OptionBOX/Info') 
                        DataPTR2_id = H5D_OPEN(file_id, '/AddTraces/OptionBOX/Data')
                endif
         endelse
endif   
CATCH, /CANCEL    

if(max(where(ErrCode eq 1)) lt -0.5) then  info[3]= '/PTR-Reaction/TwData'
if(max(where(ErrCode eq 2)) lt -0.5) then   info[4]= '/PTR-Instrument/TwData'
if(max(where(ErrCode eq 3)) lt -0.5) then  info[5]= '/PTR-Misc/TwData'
if(max(where(ErrCode eq 4)) lt -0.5 and var_exists(file_id2) lt -0.5) then   info[3]= '/AddTraces/PTR-Reaction/TwData'
if(max(where(ErrCode eq 5)) lt -0.5 and var_exists(file_id2) lt -0.5) then  info[4]= '/AddTraces/PTR-Instrument/TwData'
if(max(where(ErrCode eq 6)) lt -0.5 and var_exists(file_id2) lt -0.5) then  info[5]= '/AddTraces/PTR-Misc/TwData'
if(max(where(ErrCode eq 7)) lt -0.5 and var_exists(file_id2) lt -0.5) then   info[3]= '/AddTraces/PTR-Reaction/Data'
if(max(where(ErrCode eq 8)) lt -0.5 and var_exists(file_id2) lt -0.5) then  info[4]= '/AddTraces/PTR-Instrument/Data'
if(max(where(ErrCode eq 9)) lt -0.5 and var_exists(file_id2) lt -0.5) then  info[5]= '/AddTraces/PTR-Misc/Data'
if(max(where(ErrCode eq 10)) lt -0.5 and var_exists(file_id2) lt -0.5) then  info[6]= '/AddTraces/OptionBOX/TwData'
if(max(where(ErrCode eq 11)) lt -0.5 and var_exists(file_id2) lt -0.5) then  info[6]= '/AddTraces/OptionBOX/Data'
WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Text_info'), sET_VALUE=info
             
if(var_exists(file_id2) gt -0.5) then print, 'Old data structure (Engineering data in separate HDF5 file)'
if(max(where(ErrCode eq 3)) gt -0.5 OR  max(where(ErrCode eq 6)) gt -0.5 OR  max(where(ErrCode eq 9)) gt -0.5 ) then option=0 else option=1
;if((option eq 0 and  (max(where(ErrCode eq 10)) gt -0.5 OR  max(where(ErrCode eq 11)) gt -0.5))) then option=0 else option=1
if(option eq 1) then  print,'Extended dataset (Option box attached)' else print,'Basic dataset (No Option box)'
   



;retrieve Extractions, SampleInterval, Cycles,  PoisDead, SumSpec, and Buftimes


       n_attr=H5A_GET_NUM_ATTRS(file_id)
       mistt='nix'
       for j=0,n_attr-1 do if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'HDF5 File Creation Time') gt -0.5) then mistt=H5A_read( H5A_OPEN_IDX(file_id,j))
       if(strpos(mistt ,'nix') eq 0) then begin  ;TOF1000
                for j=0,n_attr-1 do begin
                        if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'FileCreatedTimeSTR') gt -0.5) then CreationTime=H5A_read( H5A_OPEN_IDX(file_id,j))
                        if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'Single Spec Duration (ms)') gt -0.5) then CycleTime=H5A_read( H5A_OPEN_IDX(file_id,j))
                        if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'Timebin width (ps)') gt -0.5) then SampleInterval=H5A_read( H5A_OPEN_IDX(file_id,j))
                        if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'Pulsing Period (ns)') gt -0.5) then SingleSpectrumTime=H5A_read( H5A_OPEN_IDX(file_id,j))
               endfor
               CycleTime=float(CycleTime)/1000  ; in seconds
               SampleInterval=max(float(SampleInterval)/1e12) ; in seconds
               SingleSpectrumTime=float(SingleSpectrumTime)/1000 ;in microseconds
               poisCor=1
               PoisDead=1
               Extractions=max(CycleTime*1e6/SingleSpectrumTime)
               
               BufTimes= H5D_READ(BufTimes_id)
               SumSpec= H5D_READ(SumSpec_id)
               BufTimes=BufTimes[3,*]
               SumSpec=SumSpec* max(size(buftimes,/dimensions))
               
        endif else begin ; TOF8000, QiTOF
                ; Can be improved later. For now I keep using the old working code (Sep 2015):
                FileStruct=H5_parse(File)
                Extractions=Filestruct.NbrWaveforms & extractions=max(extractions._data)
                SingleIonSignal=Filestruct.FullSpectra.Single_Ion_Signal._data; ---MB
                CreationTime=FileStruct.HDF5_FILE_CREATION_TIME & CreationTime=CreationTime._data
                SampleInterval=FileStruct.fullspectra & SampleInterval=SampleInterval.sampleinterval & SampleInterval=max(SampleInterval._data)
                poisCor=FileStruct.RawData 
                FileStruct=0
                if(strpos(strjoin(tag_names(poisCor)),'HPTC') gt -0.5) then begin
                poisCor=poisCor.HPTDC & PoisDead=poisCor.PoissonTdcDeadTime & PoisDead=max(PoisDead._data)
                poisCor=poisCor.PoissonCorrect  & poisCor=max(poisCor._data)
                endif else begin
                        poisCor=0 ;---MB
                        PoisDead=0 ;---MB
                endelse
                
                
                BufTimes= H5D_READ(BufTimes_id)
                SumSpec= H5D_READ(SumSpec_id)
                SumSpec=SumSpec*(0.2/SingleIonSignal[0]) ;---MB

                ;rearrange 4-dimensional HD5 data to 2 dim array
                mist=size(BufTimes,/dimensions)
                mist2=size(BufTimes,/n_dimensions)
                mist3=1
                for i=0,mist2-1 do mist3=mist3*mist[i]
                BufTimes=Reform(BufTimes,mist3,/overwrite)  
                timestep=median(-BufTimes[0:max(d(BufTimes))-2]+BufTimes[1:max(d(BufTimes))-1])
                BufTimes=dindgen(max(d(BufTimes)))*timestep
 
                
                
        endelse
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



      ;cause an error if highest signal is lower than MIN_SIG
        MIN_SIG=222*max(BufTimes)
        crash=sumspec[where(sumspec-MIN_SIG gt 0)]
        crash=0
 

    
if(var_exists(NamesPTR1_id) gt -0.5) then NamesPTR1= H5D_READ(NamesPTR1_id)
if(var_exists(NamesPTR1_id) gt -0.5) then DataPTR1= H5D_READ(DataPTR1_id)
if(var_exists(NamesPTR2_id) gt -0.5) then NamesPTR2= H5D_READ(NamesPTR2_id)
if(var_exists(NamesPTR2_id) gt -0.5) then DataPTR2= H5D_READ(DataPTR2_id)
if(var_exists(NamesPTR3_id) gt -0.5) then NamesPTR3= H5D_READ(NamesPTR3_id)
if(var_exists(NamesPTR3_id) gt -0.5) then DataPTR3= H5D_READ(DataPTR3_id)
        
if(var_exists(NamesPTR1_id) gt -0.5) then DataPTR1= Rearrange(DataPTR1)
if(var_exists(NamesPTR2_id) gt -0.5) then DataPTR2= Rearrange(DataPTR2)
if(var_exists(NamesPTR3_id) gt -0.5) then DataPTR3=Rearrange(DataPTR3)

;if(option eq 1) then begin
;        ;fix for sonnblick data
;        mist9=size(DataPTR2,/dimensions)
;        if( mist9[0] gt 32) then if( total(DataPTR2[0:31,*]) eq 0) then DataPTR2=DataPTR2[32:*,*]
;        ;fix for data measured without the Option-box (i.e. FILTER data)
;        if( total(DataPTR2) eq 0) then if(mist9[0] ge 2) then  DataPTR2=DataPTR2[0:1,*] else DataPTR2=DataPTR2[0,*]
;endif
if(var_exists(NamesPTR1_id)+var_exists(NamesPTR2_id)+var_exists(NamesPTR3_id) gt -2.5) then begin
        if(var_exists(NamesPTR1_id) gt -0.5) then begin
                if(var_exists(NamesPTR2_id) gt -0.5) then begin
                        if(var_exists(NamesPTR3_id) gt -0.5) then begin
                                data=[DataPTR1,DataPTR2,DataPTR3]
                                NamesPTR=[NamesPTR1,NamesPTR2,NamesPTR3]
                        endif else begin 
                                data=[DataPTR1,DataPTR2]
                                NamesPTR=[NamesPTR1,NamesPTR2]
                        endelse
                endif else begin
                        if(var_exists(NamesPTR3_id) gt -0.5) then begin
                                data=[DataPTR1,DataPTR3]
                                NamesPTR=[NamesPTR1,NamesPTR3]
                        endif else begin 
                                data=[DataPTR1]
                                NamesPTR=[NamesPTR1]
                        endelse
                endelse
                 cycles=size(DataPTR1,/dimensions)
                 cycles=cycles[1]
        endif else begin
                if(var_exists(NamesPTR2_id) gt -0.5) then begin
                        if(var_exists(NamesPTR3_id) gt -0.5) then begin
                                data=[DataPTR2,DataPTR3]
                                NamesPTR=[NamesPTR2,NamesPTR3]
                        endif else begin 
                                data=[DataPTR2]
                                NamesPTR=[NamesPTR2]
                        endelse
                endif else begin
                        data=[DataPTR3]
                        NamesPTR=[NamesPTR3]
                endelse
        endelse
 endif
 if(size(NamesPTR,/n_dimensions) eq 2) then NamesPTR=strjoin(transpose(NamesPTR),' ')
 
 if(var_exists(cycles) lt -0.5 ) then begin  
        cycles= max(size(BufTimes,/dimensions))
 endif else  if (cycles gt max(size(BufTimes,/dimensions))) then begin
                cycles= max(size(BufTimes,/dimensions))
        endif 
 if(var_exists(data) lt -0.5 ) then begin  ; crash prevention in case of no engineering data...
          data=transpose([[findgen(cycles)],[findgen(cycles)]])
          data=data[*,0:cycles-1]
          NamesPTR=['cycles2','cycles3']
 endif
 
 
 BufTimes=BufTimes[0:cycles-1]
 BufTimes=reform(buftimes,1,cycles,/overwrite)
 ;t09, time in days since 1.1.2009, 00:00:00
 StartTime=t09(CreationTime)
 TimeRow=StartTime+BufTimes/86400
 ;reaction time, E/N
 filtD=where(strmatch(NamesPTR,'p_drift*') eq 1 or strmatch(NamesPTR,'p-Drift*') eq 1)
 filtU1=where(strmatch(NamesPTR,'Udrift*') eq 1)
 filtU2=where(strmatch(NamesPTR,'Uq*') eq 1 or strmatch(NamesPTR,'Udx*') eq 1)
 filtT=where(strmatch(NamesPTR,'Drift_Temperature*') eq 1 or strmatch(NamesPTR,'T-Drift*') eq 1)
 Cyc=reform(fltarr(cycles),1,cycles,/overwrite)
 if(max(filtD) gt -0.5) then pdrift=data[filtD,*] else pdrift=cyc+max(getpar('p_drift_default'))
 if(max(filtU1) gt -0.5) then begin 
        mistUD=data[filtU1,*]
        mistUD=mistUD[0,*]
 endif else  mistUD=cyc+max(getpar('u_drift_default'))
 if(max(filtU2) gt -0.5) then  udrift=mistUD+data[filtU2,*]  else udrift=mistUD+max(getpar('udx_default'))
 if(max(filtT) gt -0.5) then  tdrift=data[filtT,*]+273.15 else begin
  tdrift=cyc+max(getpar('t_drift_default'))
   endelse
 pdrift=pdrift[0,*]
 udrift=udrift[0,*]
 tdrift=tdrift[0,*]
 ;PARAMETERS
 
 d=max(getpar('reactionlength'))  ;reactionlength in cm
 mu0=max(getpar('reduced_mobility')) ;reduced mobility of H3O+ in N2

 mu=mu0*(1013.25/pdrift)*(tdrift/273.15) ;[=] cm2/Vs 
 trxn=d/(((mu * udrift)/d))  ; reaction time in s  
        Nmolec=24.63*298*pdrift/(1013.25 *tdrift) 
        EoverN=1E17*(udrift/d)/(Nmolec*1E18)

        names=['JulianDate','ReactionTime[s]','N [1E18 molec/cm3]','EoverN',NamesPTR]

        ;remove commas'from the names
        inde=where(strpos(names,',') gt 0)
        if(max(inde) gt 0) then begin 
                posi=strpos(names[inde],',')
                length=max(size(posi,/dimensions))
                for o=0,length-1 do begin 
                        mist=names[inde[o]]
                        strput, mist,'_', posi[o]
                        names[inde[o]]=mist
                endfor
        endif

        data=[TimeRow,trxn,Nmolec,EoverN,data]




        s1=CREATE_STRUCT('names',names,'data',transpose(data),'FileName',File,'SumSpec',SumSpec,'cycles',cycles,'duration',max(BufTimes),'pdrift',pdrift,'udrift',udrift,'tdrift',tdrift,'StartTime',StartTime,'SampInt',SampleInterval,'extractions',extractions,'PoisCor',poiscor,'PoisDead',poisdead,'instrument',instrument)

        H5d_CLOSE, BufTimes_id
        H5d_CLOSE, SumSpec_id
        
        if(var_exists(NamesPTR1_id) gt -0.5) then H5d_CLOSE, NamesPTR1_id
        if(var_exists(NamesPTR1_id) gt -0.5) then H5d_CLOSE, DataPTR1_id
        if(var_exists(NamesPTR2_id) gt -0.5) then H5d_CLOSE, NamesPTR2_id
        if(var_exists(NamesPTR2_id) gt -0.5) then H5d_CLOSE, DataPTR2_id
        if(var_exists(NamesPTR3_id) gt -0.5) then H5d_CLOSE, NamesPTR3_id
        if(var_exists(NamesPTR3_id) gt -0.5) then H5d_CLOSE, DataPTR3_id
        H5f_CLOSE,  file_id 
        if(var_exists(file_id2) gt -0.5) then H5f_CLOSE,  file_id2 



Return, s1

end 



function GetFileExport, event, destfolder, Name1

   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_6'), get_uVALUE= massFL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_7'), get_uVALUE= massUL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_1'), get_uVALUE= cpsFL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_2'), get_uVALUE= corrcpsFL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_3'), get_uVALUE= cpsUL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_4'), get_uVALUE= corrcpsUL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_5'), get_uVALUE= ppbFL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_6'), get_uVALUE= corrppbFL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_10'), get_uVALUE= ppbUL 
   WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_12'), get_uVALUE= corrppbUL 
   if(max([max(cpsFL),max(corrcpsFL),max(cpsUL),max(corrcpsUL)]) eq -9999) then begin 
          massUL=readCsv(DestFolder+'Export\UL\IonList\MassIDs_'+Name1+'UL.csv')
          massFL=readCsv(DestFolder+'Export\FL\IonList\MassIDs_'+Name1+'FL.csv')
          corrcpsUL=readFloat(DestFolder+'Export\UL\cps\ocorr\corrcps'+Name1+'UL.fdt')
          cpsUL=readFloat(DestFolder+'Export\UL\cps\cps'+Name1+'UL.fdt')
          corrcpsFL=readFloat(DestFolder+'Export\FL\cps\ocorr\corrcps'+Name1+'FL.fdt')
          cpsFL=readFloat(DestFolder+'Export\FL\cps\cps'+Name1+'FL.fdt')
          corrppbUL=readFloat(DestFolder+'Export\UL\ppb\ocorr\corrppb'+Name1+'UL.fdt')
          ppbUL=readFloat(DestFolder+'Export\UL\ppb\ppb'+Name1+'UL.fdt')
          corrppbFL=readFloat(DestFolder+'Export\FL\ppb\ocorr\corrppb'+Name1+'FL.fdt')
          ppbFL=readFloat(DestFolder+'Export\FL\ppb\ppb'+Name1+'FL.fdt')
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_6'), set_uVALUE= massFL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'Label_7'), set_uVALUE= massUL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_1'), set_uVALUE= cpsFL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_2'), set_uVALUE= corrcpsFL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_3'), set_uVALUE= cpsUL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_4'), set_uVALUE= corrcpsUL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_5'), set_uVALUE= ppbFL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_6'), set_uVALUE= corrppbFL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_10'), set_uVALUE= ppbUL 
          WIDGET_CONTROL, WIDGET_INFO(event.TOP, FIND_BY_UNAME = 'base_12'), set_uVALUE= corrppbUL 
  endif
  return, create_struct('massUL',massUL,'massFL',massFL,'corrcpsUL',corrcpsUL,'cpsUL',cpsUL,'corrcpsFL',corrcpsFL,$
    'cpsFL',cpsFL,'corrppbUL',corrppbUL,'ppbUL',ppbUL,'corrppbFL',corrppbFL,'ppbFL',ppbFL)
end




 function getPrimIons ,masses,cps,mode                         
  if (mode eq 0) then begin ;H3O+   m21factor = 1/0.206% = 487 m38factor = 99.926/0.074 = 1350 , m39factor = 99.588/0.412 = 242
          Prim1=cps[where(abs(masses-21.0221) eq min(abs(masses-21.0221))),*]*487 
          if(getpar('m38') eq 1) then  Prim2=cps[where(abs(masses-38.0326) eq min(abs(masses-38.0326))),*]*1350 else Prim2=cps[where(abs(masses-39.0327) eq min(abs(masses-39.0327))),*]*242       
          if(min(abs(masses-38.0326)) gt 0.005) then begin
                  Prim2=Prim2-Prim2
                  print, 'Warning: m38.033 not detected'
           endif
           Prim1=Prim1[0,*]
           Prim2=Prim2[0,*]
   endif
   if (mode eq 5) then begin ;NO+
          Prim1=cps[where(abs(masses-30.994 ) eq min(abs(masses-30.994 ))),*]*269
          Prim2=cps[where(abs(masses-47.9966) eq min(abs(masses-47.9966))),*]*242 
          Prim1=Prim1[0,*]
          Prim2=Prim2[0,*]
  endif
  if (mode eq 9) then begin ;O2+
          Prim1=cps[where(abs(masses-33.9935 ) eq min(abs(masses-33.9935 ))),*]*242
          Prim2=Prim1-Prim1   
          Prim1=Prim1[0,*]
          Prim2=Prim2[0,*]
   endif     
  Return, create_struct('A',Prim1,'B',Prim2)
 
end                             
 


function GetSumSpec, File
;a=H5_BROWSER(File)

misst=systime(1)
;print, 'time',  systime(1)-misst
s1=CREATE_STRUCT('duration',-9999)

;ErrCode=0
sumspec=-9999

;CATCH, Error_status 
;IF Error_status NE 0 THEN BEGIN 
 ;     PRINT, 'Error index: ', Error_status 
;      PRINT, 'Error message: ', !ERROR_STATE.MSG 
;      ErrCode=1
;      CATCH, /CANCEL 
;ENDIF 

; old condition: h5d_get_storage_size(dataptr1_id) gt 10
;if(ErrCode eq 0) then begin

file_id = H5f_OPEN(File)    
info=strarr(7)
info[0]='H5 OPEN:'
 T1000=-1
  CATCH, Error_status2 
  IF Error_status2 NE 0 THEN  begin
   T1000=1
  endif
  if (T1000 eq -1) then begin
          BufTimes_id = H5D_OPEN(file_id, '/TimingData/BufTimes')
          info[1]='/TimingData/BufTimes'
          SumSpec_id = H5D_OPEN(file_id, '/FullSpectra/SumSpectrum')
          info[2]='/FullSpectra/SumSpectrum'
          instrument='TOF8000'
  endif
  if (T1000 eq 1) then begin
          T1000=-1
          BufTimes_id = H5D_OPEN(file_id, '/SPECdata/Times')
          info[1]='/SPECdata/Times'
          SumSpec_id = H5D_OPEN(file_id, '/SPECdata/AverageSpec')
          info[2]='/SPECdata/AverageSpec'
          instrument='TOF1000'
  endif
  CATCH, /CANCEL 
  

ErrCode=0
CATCH, Error_status 
  IF Error_status NE 0 THEN BEGIN 
        PRINT, 'Error index: ', Error_status 
        PRINT, 'Error message: ', !ERROR_STATE.MSG 
        ErrCode=1
        CATCH, /CANCEL 
  ENDIF 
if(ErrCode eq 0) then begin

;retrieve Extractions, SampleInterval, Cycles,  PoisDead, SumSpec, and Buftimes


       n_attr=H5A_GET_NUM_ATTRS(file_id)
       mistt='nix'
       for j=0,n_attr-1 do if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'HDF5 File Creation Time') gt -0.5) then mistt=H5A_read( H5A_OPEN_IDX(file_id,j))
       if(strpos(mistt ,'nix') eq 0) then begin  ;TOF1000
                for j=0,n_attr-1 do begin
                        if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'FileCreatedTimeSTR') gt -0.5) then CreationTime=H5A_read( H5A_OPEN_IDX(file_id,j))
                        if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'Single Spec Duration (ms)') gt -0.5) then CycleTime=H5A_read( H5A_OPEN_IDX(file_id,j))
                        if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'Timebin width (ps)') gt -0.5) then SampleInterval=H5A_read( H5A_OPEN_IDX(file_id,j))
                        if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'Pulsing Period (ns)') gt -0.5) then SingleSpectrumTime=H5A_read( H5A_OPEN_IDX(file_id,j))
               endfor
               CycleTime=float(CycleTime)/1000  ; in seconds
               SampleInterval=max(float(SampleInterval)/1e12) ; in seconds
               SingleSpectrumTime=float(SingleSpectrumTime)/1000 ;in microseconds
               poisCor=1
               PoisDead=1
               Extractions=max(CycleTime*1e6/SingleSpectrumTime)
               
               BufTimes= H5D_READ(BufTimes_id)
               SumSpec= H5D_READ(SumSpec_id)
               BufTimes=BufTimes[3,*]
               SumSpec=SumSpec* max(size(buftimes,/dimensions))
               
        endif else begin ; TOF800, QiTOF
        
              ;    for j=0,n_attr-1 do begin
            ;            if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'NbrWaveforms') gt -0.5) then Extractions=H5A_read( H5A_OPEN_IDX(file_id,j))
            ;           if(strpos(H5A_GET_NAME( H5A_OPEN_IDX(file_id,j)),'HDF5 File Creation Time') gt -0.5) then CreationTime=H5A_read( H5A_OPEN_IDX(file_id,j))
           ;    endfor
       
        
        
                ; Can be improved later. For now I keep using the old working code (Sep 2015):
              FileStruct=H5_parse(File)
                Extractions=Filestruct.NbrWaveforms & extractions=max(extractions._data)
                CreationTime=FileStruct.HDF5_FILE_CREATION_TIME & CreationTime=CreationTime._data
                SampleInterval=FileStruct.fullspectra & SampleInterval=SampleInterval.sampleinterval & SampleInterval=max(SampleInterval._data)
                SingleIonSignal=Filestruct.FullSpectra.Single_Ion_Signal._data ;---MB

                poisCor=FileStruct.RawData 
                FileStruct=0
                if(strpos(strjoin(tag_names(poisCor)),'HPTC') gt -0.5) then begin
                poisCor=poisCor.HPTDC & PoisDead=poisCor.PoissonTdcDeadTime & PoisDead=max(PoisDead._data)
                poisCor=poisCor.PoissonCorrect  & poisCor=max(poisCor._data)
                endif else begin
                        poisCor=1
                        PoisDead=1
                endelse
                
                   
                BufTimes= H5D_READ(BufTimes_id)
                SumSpec= H5D_READ(SumSpec_id)
                SumSpec=SumSpec*(0.2/SingleIonSignal[0]) ;---MB

                ;rearrange 4-dimensional HD5 data to 2 dim array
                mist=size(BufTimes,/dimensions)
                mist2=size(BufTimes,/n_dimensions)
                mist3=1
                for i=0,mist2-1 do mist3=mist3*mist[i]
                BufTimes=Reform(BufTimes,mist3,/overwrite)  
                timestep=median(-BufTimes[0:max(d(BufTimes))-2]+BufTimes[1:max(d(BufTimes))-1])
                BufTimes=dindgen(max(d(BufTimes)))*timestep
 
                
                
        endelse
   















;BufTimes= H5D_READ(BufTimes_id)
;SumSpec= H5D_READ(SumSpec_id)



;t09, time in days since 1.1.2009, 00:00:00

StartTime=t09(CreationTime)

;cause an error if highest signal is lower than MIN_SIG
MIN_SIG=222*max(BufTimes)
crash=sumspec[where(sumspec-MIN_SIG gt 0)]
crash=0

cycles=size(buftimes,/n_elements)

dur = max(BufTimes)*cycles/(cycles-1); ---MB

s1=CREATE_STRUCT('FileName',File,'SumSpec',SumSpec,'cycles',cycles,'duration',dur,'StartTime',StartTime,'SampInt',SampleInterval,'extractions',extractions,'PoisCor',poiscor,'PoisDead',PoisDead,'instrument',instrument)


H5d_CLOSE, SumSpec_id
H5d_CLOSE, BufTimes_id
H5f_CLOSE,  file_id 


endif else begin
print, 'HDF5 FILE CORRUPTED: ', File
endelse



Return, s1
end


 function getpar ,par_name
      par_name='_'+par_name+'='
      OPENR, lun, 'C:/PTRwid/parfile.txt', /GET_LUN
      path=strarr(1)
      READF, lun, path
      Free_Lun, lun
      PTRwidpar=readcsvstr(path) 
      row_index=where(stregex(PTRwidpar,par_name) gt -0.5) 
      line_index=stregex(PTRwidpar,'=')
      line_index=line_index[row_index]+1
      value=float(strmid(PTRwidpar[row_index],line_index,99))
 return, value
 end     
 

function isodist, lib
C12p=0.989
C13p=0.011
H1p=1
O16p=0.9976
O17p=0.0004
O18p=0.0020
N14p=0.9963
N15p=0.0037
H_posp=1
e_negp=1
Cl35p=0.7577
Cl37p=0.2423
Fp=1
S32p=0.9504
S33p=0.0075
S34p=0.0421

C12=12
C13=13.003355
H1=1.007825
O16=15.994915
O17=16.999131
O18=17.99916
N14=14.003074
N15=15.000108
H_pos=1.007276467
e_neg=-0.000548533
Cl35=34.968852
Cl37=36.965903
F=18.998403
S32O4=31.97207+4*O16
S33O4=32.971456+4*O16
S34O4=33.967866+4*O16


nC=lib[0]+lib[1]
nN=lib[6]+lib[7]
nCl=lib[10]+lib[11]

P13C=nC*C13p*C12p^(nC-1)
P15N=nN*N15p*N14p^(nN-1)
P37Cl=nCl*Cl37p*Cl35p^(nCl-1)


masses=0
disp=0
for c=0,1 do for nnn=0,1 do for cl=0,1 do begin
    if(c le nC) then if(nnn le nN) then if (cl le nCl) then begin
        n=[nC-c,c,lib[2:5],nN-nnn,nnn,lib[8:9],nCl-cl,cl,lib[12:14]]
        entry=C12*n[0]+C13*n[1]+H1*n[2]+O16*n[3]+O17*n[4]+O18*n[5]+N14*n[6]+N15*n[7]+Cl35*n[10]+Cl37*n[11]+F*n[12]+S32O4*n[13]+S34O4*n[14]+H_pos*n[8]+e_neg*n[9]
        if(nC ge 1) then if(c eq 1) then PC= P13C else PC=(1- P13C) else PC=1
        if (nN ge 1) then if(nnn eq 1) then PN= P15N else PN=(1- P15N) else PN=1
        if (nCl ge 1) then if(cl eq 1) then PCl= P37Cl else PCl=(1- P37Cl) else PCl=1
        Ptot=PC*PN*PCl
        disp=[disp,Ptot]
        masses=[masses,entry]
    endif
endfor
masses=masses[where(disp ge 0.002)]
disp=disp[where(disp ge 0.002)]
disp=disp[sort(masses)]
masses=masses[sort(masses)]

;print, transpose([[masses],[disp]])

S1=create_struct('masses',masses,'dist',disp)
return, S1
end

function LoadMassRange, file,MassLow, MassHigh, a,t0,ex,SampInt
ErrCode=0
ptrData=-9999
locatie='/FullSpectra/TofData'

;--- MB removed this section because it didn't work for our ADC:
;CATCH, Error_status 
;IF Error_status NE 0 THEN BEGIN 
;       if(strpos(locatie,'/SPECdata/Intensities') eq 0) then ErrCode=1 else locatie='/SPECdata/Intensities'
;      if(ErrCode eq 1) then begin
;              PRINT, 'Error index: ', Error_status 
;              PRINT, 'Error message: ', !ERROR_STATE.MSG 
;              CATCH, /CANCEL 
;      endif
;ENDIF 
if(ErrCode eq 0) then begin

  fileid=H5f_OPEN(File)
  dataset_id1 = H5D_OPEN(fileid, locatie) 
  dataspace_id1 = H5D_GET_SPACE(dataset_id1) 
  Dims=H5S_GET_SIMPLE_EXTENT_DIMS(Dataspace_id1) 
  ptrData = ptrarr(1)

  Start =floor(m2t(MassLow,a,t0,ex,SampInt)) 
  Width = ceil(m2t(MassHigh,a,t0,ex,SampInt)- Start)
  mist2=start+width
 
  if (max(size(Dims,/dimensions)) ge 2) then begin
          ddd=lonarr(size(Dims,/dimensions)-1)
          eee=ddd
          for i=1,max(size(Dims,/dimensions))-1 do eee[i-1]=Dims[i]
          start2 = [Start, ddd] 
          count = [Width,eee] 
  endif else begin
          start2 = [Start] 
          count = [Width] 
  endelse
 
  FileStruct=H5_parse(File)
  SingleIonSignal=Filestruct.FullSpectra.Single_Ion_Signal._data ;---MB

  H5S_SELECT_HYPERSLAB, dataspace_id1, start2, count,  /RESET 
  memory_space_id1 = H5S_CREATE_SIMPLE(count) 
  ptrData=Ptr_New(rearrange(H5D_READ(dataset_id1, FILE_SPACE=dataspace_id1, MEMORY_SPACE=memory_space_id1)),/No_Copy) ; Data
  ;ptrData=ptrData*(0.2/SingleIonSignal[0]) ;---MB 

  H5S_CLOSE, memory_space_id1
  H5d_CLOSE, dataset_id1
  H5S_CLOSE, dataspace_id1 
  H5f_CLOSE,  fileid
   
endif else begin
ptrData=Ptr_New(-9999)
print, 'HDF5 FILE CORRUPTED 3'
endelse
 
Return, ptrData 

end





function m2t, value, a, t0, ex,SampInt, time=time, mass=mass

  ; convert tof- time scale to tof mass scale and vice versa
  help, sampint, output=helpi
  if (strpos(helpi, 'Undefined') gt -0.5) then SampInt=1e-10

  ;if(min(value) gt 10000) then   value2=((float(value)*SampInt/1e-10-t0)/a)^(1/ex)  else  value2=long(a*value^ex+t0)*1e-10/SampInt
  if (keyword_set(time)) then begin
 ;   print,'forced time to mass conversion'
  endif
if (keyword_set(mass)) then begin
 ;   print,'forced mass to time conversion'
  endif

if (keyword_set(time)) then value2=exp(alog((float(value)*SampInt/1e-10-t0)/a)/ex) else $
    if (keyword_set(mass)) then value2=(a*value^ex+t0)*1e-10/SampInt else $
    if(min(value) gt 3500) then   value2=exp(alog((float(value)*SampInt/1e-10-t0)/a)/ex)    else $
    value2=(a*value^ex+t0)*1e-10/SampInt



  return, value2
end  


pro MakeCsv, name, data 
    
    openw, lun, name , /get_lun, width=1234567
   
    sizex = size(data, /dimensions)
    ;stringx = REFORM(string(data,  FORMAT='(E13.6)'),sizex[0],sizex[1], /OVERWRITE) 
    
    stringx = strtrim(data,2) ;removing of blanks 
    if (sizex[0] gt 1.5) then stringx[0:sizex[0]-2, *] = stringx[0:sizex[0]-2, *] + ',' ; makes commas between columns names
   
    printf, lun, stringx
    close, lun, /file
    free_lun, lun
   
end




pro saveSumSpecs, folder, name, Names, Filepar, peaks, a, t0, ex, minSpec, maxSpec

  ;destfolder+Names(i)+'_corrected.hdf5'
  file = folder+name+'.hdf5'
  fid = H5F_CREATE(file)

  data = minSpec
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'SumSpecMin',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data
  
  data = maxSpec
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'SumSpecMax',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  data = a
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'A',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data
  
  
  data = t0
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'T0',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data
  
  
  data = ex
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'Ex',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  data = peaks
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'MassList',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data
  
  data = FilePar.PeakShape
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'Peakshape',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data
  
  data = FilePar.PeakShapeX
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'PeakshapeX',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  data = Names
  datatype_id = H5T_IDL_CREATE(data)
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'FileNames',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data
  
  data = FilePar.ResolutionVsMass
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'Resolution',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  data = FilePar.StdResolutionVsMass
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'StdResolution',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  n = size(Names, /DIMENSIONS)
  for i=0,n[0]-1 do begin
    file = folder+Names(i)+'_corrected.hdf5'
    file_id2 = H5F_OPEN(file)
    dataset_id1 = H5D_OPEN(file_id2, '/SumSpectrum')
    sumSpec = H5D_read(dataset_id1)
    dataset_id1 = H5D_OPEN(file_id2, '/Baseline')
    Baseline = H5D_read(dataset_id1)

    data = sumSpec
    
    if (i eq 0) then begin
      avgSpectrum = sumSpec
      datatype_id = H5T_IDL_CREATE(float(data))
      s1 = [size(data,/DIMENSIONS), n[0]]
      dataspace_id = H5S_CREATE_SIMPLE(s1)
      start = [0,0]
      count = [size(data, /DIMENSIONS), 1]
      h5s_select_hyperslab, dataspace_id, start, count, /reset
      dataset_id = H5D_CREATE(fid,'SumSpecs',datatype_id,dataspace_id)
      dataset_id_for_Attributes = dataset_id
      ds1 =  h5s_create_simple(size(data,/DIMENSION))
      H5D_WRITE, dataset_id, data, memory_space_id=ds1, file_space_id=dataspace_id
      
      ; store name of original raw-data file as an attribute:
      datatype_id = H5T_REFERENCE_CREATE()
      datatype_id = H5T_IDL_CREATE('thisisastring')
      dataspace_id5 = H5S_CREATE_SCALAR()
      attr_id = H5A_CREATE(dataset_id,'FileName',datatype_id,dataspace_id5)
      H5A_WRITE,attr_id,Names[i]
      
      
      ms = size(sumSpec,/DIMENSIONS)
      data = m2t(findgen(ms[0]),FilePar.a, FilePar.t0, FilePar.ex, FilePar.sampleInterval, /time)
      datatype_id3 = H5T_IDL_CREATE(float(data))
      dataspace_id3 = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
      dataset_id3 = H5D_CREATE(fid,'MassAxis',datatype_id3,dataspace_id3)
      H5D_WRITE,dataset_id3,data
      
      
    endif else begin
      avgSpectrum = avgSpectrum*(i/(i+1.0)) + sumSpec*(1/(i+1.0))

      start = [0,i]
      h5s_select_hyperslab, dataspace_id, start, count, /reset
      ds1 =  h5s_create_simple(size(data,/DIMENSION))
      H5D_WRITE, dataset_id, data, memory_space_id=ds1, file_space_id=dataspace_id 
      
    endelse


    data = BaseLine
    if (i eq 0) then begin
      avgBaseline = BaseLine
      datatype_id = H5T_IDL_CREATE(float(data))
      s2 = [size(data,/DIMENSIONS), n[0]]
      dataspace_id2 = H5S_CREATE_SIMPLE(s2)
      start2 = [0,0]
      count2 = [size(data, /DIMENSIONS), 1]
      h5s_select_hyperslab, dataspace_id2, start2, count2, /reset
      dataset_id2 = H5D_CREATE(fid,'BaseLines',datatype_id,dataspace_id2)
      ds2 =  h5s_create_simple(size(data,/DIMENSION))
      H5D_WRITE, dataset_id2, data, memory_space_id=ds2, file_space_id=dataspace_id2
    endif else begin
      avgBaseline = avgBaseline*(i/(i+1.0)) + BaseLine*(1/(i+1.0))

      start2 = [0,i]
      h5s_select_hyperslab, dataspace_id2, start2, count2, /reset
      ds2 =  h5s_create_simple(size(data,/DIMENSION))
      H5D_WRITE, dataset_id2, data, memory_space_id=ds2, file_space_id=dataspace_id2
    endelse
    


  endfor
  

  
  
  
  ; create some attributes:
  dataset_id = dataset_id_for_Attributes
  datatype_id = H5T_REFERENCE_CREATE()
  datatype_id = H5T_IDL_CREATE(3.141592)
  dataspace_id = H5S_CREATE_SCALAR()
  attr_id = H5A_CREATE(dataset_id,'A',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,filePar.a

  attr_id = H5A_CREATE(dataset_id,'T0',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,filePar.t0

  attr_id = H5A_CREATE(dataset_id,'Ex',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,FilePar.Ex

  ;attr_id = H5A_CREATE(dataset_id,'Resolution',datatype_id,dataspace_id)
  ;H5A_WRITE,attr_id,FilePar.Resolution

  attr_id = H5A_CREATE(dataset_id,'SampleInterval',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,FilePar.sampleInterval

  attr_id = H5A_CREATE(dataset_id,'IntegrationTime',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,FilePar.integrationTime


  ;Average Spectrum:
  data = avgSpectrum
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'AvgSpectrum',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  ;Average Baseline:
  data = avgBaseline
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'AvgBaseline',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data


  H5D_CLOSE,dataset_id
  H5S_CLOSE,dataspace_id
  H5T_CLOSE,datatype_id
  H5F_CLOSE,fid
  



end

pro saveSpec, folder, name, spectrum, filePar, sampInt, duration
  file = folder+name+'_corrected.hdf5'
  fid = H5F_CREATE(file)

  data = FilePar.PeakShape
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'Peakshape',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  data = FilePar.ResolutionVsMass
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'Resolution',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  data = filePar.baseline
  data = data/duration ;save in units of counts per second per bin
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'Baseline',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  data = spectrum
  data = data/duration
  datatype_id = H5T_IDL_CREATE(float(data))
  dataspace_id = H5S_CREATE_SIMPLE(size(data,/DIMENSIONS))
  dataset_id = H5D_CREATE(fid,'SumSpectrum',datatype_id,dataspace_id)
  H5D_WRITE,dataset_id,data

  ; create some attributes:
  datatype_id = H5T_REFERENCE_CREATE()
  datatype_id = H5T_IDL_CREATE(3.141592)
  dataspace_id = H5S_CREATE_SCALAR()
  attr_id = H5A_CREATE(dataset_id,'A',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,filePar.a

  attr_id = H5A_CREATE(dataset_id,'T0',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,filePar.t0

  attr_id = H5A_CREATE(dataset_id,'Ex',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,FilePar.Ex

  attr_id = H5A_CREATE(dataset_id,'Resolution',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,FilePar.Resolution

  attr_id = H5A_CREATE(dataset_id,'SampleInterval',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,sampInt

  attr_id = H5A_CREATE(dataset_id,'IntegrationTime',datatype_id,dataspace_id)
  H5A_WRITE,attr_id,duration

  H5D_CLOSE,dataset_id
  H5S_CLOSE,dataspace_id
  H5T_CLOSE,datatype_id
  H5F_CLOSE,fid 
end


pro makeFloat, name, data 
data=reform(data)
    n_dim = size(data, /n_dimensions)
    dim =  size(data, /dimensions)
    ind=1
    for i=0,n_dim-1 do ind=ind*dim[i]
    openw, lun, name , /get_lun
    printf, lun, [n_dim,dim,reform(data,ind)],  FORMAT='(E17.9)'
    close, lun, /file
    free_lun, lun
  ; data=reform(data,dim,/overwrite)
end

function match, mass, lib, accur, narrow=narrow

if(size(accur,/type) eq 0) then accuracy=150 else accuracy=accur
if (keyword_set(narrow)) then accuracy=10 
;print, accuracy
res=4000
C=2*SQRT(2*ALOG(2)) 
sig=mass/(res*C)      
;m_range=1000*sig ; in mDa, i.e. 106 ppm

m_range=max([2, accuracy* mass/ 1000]) ;  in mDa, i.e. the larger of 2 mDa or 150 ppm (or 10 ppm with /narrow)

mass=max(mass)
match='no match'
type=''
deviation=min(abs(lib[0,*]-mass))
indi=where(abs(lib[0,*]-mass) eq deviation)



compos=lib[*,indi]
C=compos[1]+compos[2]
H=compos[3]
O=compos[4]+compos[5]+compos[6]
N=compos[7]+compos[8]



if (C eq 0) then type=[type,'inorg']

if (C ge 1) then type=[type,'org']
if (C ge 1 and O eq 0 and N eq 0) then type=[type,'HC']
if (C ge 1  and N ge 1) then type=[type,'orgN']
if (C ge 1  and N eq 1) then type=[type,'orgN1']
if (C ge 1  and N eq 2) then type=[type,'orgN2']
if (C ge 1   and O eq 0 and N ge 1) then type=[type,'NHC']
if (C ge 1   and O eq 1 and N ge 1) then type=[type,'NHCO']
if (C ge 1   and O eq 2 and N ge 1) then type=[type,'NHCO2']
if (C ge 1   and O eq 3 and N ge 1) then type=[type,'NHCO3']
if (C ge 1   and O eq 4 and N ge 1) then type=[type,'NHCO4']
if (C ge 1   and O eq 5 and N ge 1) then type=[type,'NHCO5']
if (C ge 1   and O eq 6 and N ge 1) then type=[type,'NHCO6']
if (C ge 1   and O eq 7 and N ge 1) then type=[type,'NHCO7']
if (C ge 1   and O eq 8 and N ge 1) then type=[type,'NHCO8']
if (C ge 1  and N eq 0) then type=[type,'orgNoN']
if (C ge 1 and O ge 1  and N eq 0) then type=[type,'orgO']
if (C ge 1 and O eq 1 and N eq 0) then type=[type,'HCO']
if (C ge 1 and O eq 2 and N eq 0) then type=[type,'HCO2']
if (C ge 1 and O eq 3 and N eq 0) then type=[type,'HCO3']
if (C ge 1 and O eq 4 and N eq 0) then type=[type,'HCO4']
if (C ge 1 and O eq 5 and N eq 0) then type=[type,'HCO5']
if (C ge 1 and O eq 6 and N eq 0) then type=[type,'HCO6']
if (C ge 1 and O eq 7 and N eq 0) then type=[type,'HCO7']
if (C ge 1 and O eq 8 and N eq 0) then type=[type,'HCO8']

if (deviation lt 0.003) then match=formula(compos[0:15])

if (match eq 'no match') then type='not matched'


devi=1000*([lib[0,indi-4],lib[0,indi-3],lib[0,indi-2],lib[0,indi-1],lib[0,indi],lib[0,indi+1],lib[0,indi+2],lib[0,indi+3],lib[0,indi+4]]-mass)        



if (abs(deviation) lt 0.003 and C ge 1  and N eq 2) then begin
  inx=where(abs(devi) lt 3)
  alt3=0
  if(max(inx) ge 0) then for r=0,max(size(inx,/dimensions))-1 do begin
        compos=lib[*,indi+inx[r]-4]
        N=compos[7]+compos[8]
        if(N eq 0) then alt3=alt3+1
  endfor
  if (alt3 eq 0) then type=[type,'orgN2alt3']
endif

if (abs(deviation) lt 0.003 and C ge 1  and N eq 1) then begin
  inx=where(abs(devi) lt 3)
  alt3=0
  if(max(inx) ge 0) then for r=0,max(size(inx,/dimensions))-1 do begin
        compos=lib[*,indi+inx[r]-4]
        N=compos[7]+compos[8]
        if(N eq 0) then alt3=alt3+1
  endfor
  if (alt3 eq 0) then type=[type,'orgN1alt3']
endif


if (abs(deviation) lt 0.003 and C ge 1  and N ge 1) then begin
  inx=where(abs(devi) lt 3)
  alt3=0
  if(max(inx) ge 0) then for r=0,max(size(inx,/dimensions))-1 do begin
        compos=lib[*,indi+inx[r]-4]
        N=compos[7]+compos[8]
        if(N eq 0) then alt3=alt3+1
  endfor
  if (alt3 eq 0) then type=[type,'orgNalt3']
endif

if (abs(deviation) lt 0.003 and C ge 1  and N ge 1) then begin
  inx=where(abs(devi) lt 4)
  alt3=0
  if(max(inx) ge 0) then for r=0,max(size(inx,/dimensions))-1 do begin
        compos=lib[*,indi+inx[r]-4]
        N=compos[7]+compos[8]
        if(N eq 0) then alt3=alt3+1
  endfor
  if (alt3 eq 0) then type=[type,'orgNalt4']
endif

if (abs(deviation) lt 0.003 and C ge 1  and N ge 1) then begin
  inx=where(abs(devi) lt 5)
  alt3=0
  if(max(inx) ge 0) then for r=0,max(size(inx,/dimensions))-1 do begin
        compos=lib[*,indi+inx[r]-4]
        N=compos[7]+compos[8]
        if(N eq 0) then alt3=alt3+1
  endfor
  if (alt3 eq 0) then type=[type,'orgNalt5']
endif

candidates=''

if (max(size(where(abs(mass-lib[0,*]) lt m_range/1000),/dimensions)) gt 1) then $
candidates=[transpose(string(1000*(lib[0,where(abs(mass-lib[0,*]) lt m_range/1000)]-mass),format='(F8.3)')),formula(lib[*,where(abs(mass-lib[0,*]) lt  m_range/1000)])] else $
if (max(where(abs(mass-lib[0,*]) lt  m_range/1000)) gt -0.5) then candidates=[(string(1000*(lib[0,where(abs(mass-lib[0,*]) lt  m_range/1000)]-mass),format='(F8.3)')),formula(lib[*,where(abs(mass-lib[0,*]) lt  m_range/1000)])]  
;print, ''
;print,'candidates'
;print, candidates
;print,'__________'

s1=CREATE_STRUCT('formula',match,'devi',devi,'type',type,'candidates',candidates,'entry',compos)
return,s1
end


function multiples, vec
 res=[0,0]
 length=max(size(vec,/dimensions))


ii=long(0)

while (ii lt length) do begin
        counter=0
        if (ii lt length-1) then while (vec[ii+counter+1] eq vec[ii+counter]) do begin
                 counter=counter+1
                 if (ii+counter ge length-1) then break
        endwhile
        res=[[res],[vec[ii+counter],counter+1]]
        ii=ii+counter+1
endwhile
res=res[*,1:*]

return, transpose(res)
end



function PeakTable, masslist, resolution
 if (var_exists(resolution) lt -0.5) then resolution=4000
length=max(size(masslist,/dimensions))      
IntList=FltArr(length,6)
IntList[*,0]=masslist
 res=resolution
for i=0,length-1 do begin
        C=2*SQRT(2*ALOG(2)) 
        sig=IntList[i,0]/(res*C)              
        IntList[i,1]=IntList[i,0]-2*sig
        IntList[i,2]=IntList[i,0]+2*sig
        IntList[i,3]=sig
        if(i gt 0.5) then Down=IntList[i-1,0]*(1+2/(res*C)) else Down=0
        if(i lt length-1.5) then Up=IntList[i+1,0]*(1-2/(res*C)) else Up=100000
        if(Down gt IntList[i,1]) then begin
        
                overlap=Down-IntList[i,1]
         ;       print,'down',down, overlap*1000, IntList[i,0], IntList[i,1],IntList[i,0]-2*sig+overlap/2
                IntList[i,1]=IntList[i,0]-2*sig+overlap/2
                Intlist[i,4]=overlap/(2*sig)
                
        endif
        if(Up lt IntList[i,2]) then begin
                overlap=IntList[i,2]-Up
         ;          print,'up',up, overlap*1000, IntList[i,0], IntList[i,2],IntList[i,0]+2*sig-overlap/2
             
                IntList[i,2]=IntList[i,0]+2*sig-overlap/2
                Intlist[i,5]=overlap/(2*sig)
        endif
endfor
;print, transpose(intlist)
;intlist=intlist[where(intlist[*,2] gt 0.007+intlist[*,1]),*]

return, IntList
end


function ppm_bin, masslist, BinWidth_ppm

;create bin_scale
;bin-width is the bigger of 1mDa and the ppm-width
;i.e. BinWidth_ppm of 0.4 and below returns mDa bin scale
 
BinWidth_ppm=BinWidth_ppm/1e6

start=float(10)
startppm=min([2000,0.001*floor(1/BinWidth_ppm)])

f=where(masslist ge startppm) & if (max(f) gt -1) then m_ppm=masslist[f] else m_ppm=-1
f=where(masslist gt start and masslist lt startppm) & if (max(f) gt -1) then m_mDa=masslist[f] else m_mDa=-1


;ppm bins
if(max(m_ppm) gt -1) then begin 
        m_start=startppm*exp(BinWidth_ppm*double(lindgen(2e6)))
        m_end=[m_start[1:*],max(m_start)+1]
        filt=where(m_start lt 2000)
        m_start2=m_start[filt]
        m_end2=m_end[filt]

        length=max(size(m_start2,/dimensions))
        scores2=fltarr(length)
        I=floor(alog(m_ppm/startppm)/BinWidth_ppm)
        I=I[sort(I)]
        I2=multiples(I)
        scores2[I2[*,0]]=I2[*,1]
endif
;mDa bins

if(max(m_mDa) gt -1) then begin 
        mDa_bins=1000*(startppm-start)
        m_start=double(lindgen(mDa_bins))/1000+start
        m_end=[m_start[1:*],max(m_start)+0.001]

        length=max(size(m_start,/dimensions))
        scores=fltarr(length)
        I=floor((m_mDa-start)/0.001)
        I=I[sort(I)]
        I2=multiples(I)
        scores[I2[*,0]]=I2[*,1]
endif
if( max(m_mDa)eq -1) then begin
        m_start=m_start2 
        m_end=m_end2 
        scores=scores2
endif
if( max(m_mDa)gt -1 and max(m_ppm)gt -1 ) then begin
        m_start=[m_start,m_start2]
        m_end=[m_end,m_end2]
        scores=[scores,scores2]
endif

return, [[m_start],[m_end],[scores]]
end


function range, data
   Range=[min(data),max(data)]
  Return, Range
end


function ReadCsv, file
mistt=systime(1)
info = File_Info(File)
if(info.exists eq 1) then begin
      OPENR, lun, file, /GET_LUN
      rows=FILE_LINES(file)
      data = STRARR(rows)
      READF, lun, data
      Free_Lun, lun
      cols=size(strsplit(data[1],', ', /EXTRACT),/dimensions)
      data2=fltarr(FILE_LINES(file),cols[0]) ;
      for i=0L,max(FILE_LINES(file))-1 do begin
              cols2=size(strsplit(data[i],', ', /EXTRACT),/dimensions)  
              if cols2[0] eq cols[0] then data2[i,*]=strsplit(data[i],', ', /EXTRACT)
      endfor
endif else begin
data2=-9999
endelse
return, data2
end

function ReadCsvStr, file

info = File_Info(File)
if(info.exists eq 1) then begin
      OPENR, lun, file, /GET_LUN
      rows=FILE_LINES(file)
      data = STRARR(rows)
      READF, lun, data, format='(A30)'
      Free_Lun, lun
    
      cols=max(size(strsplit(data[1],', ', /EXTRACT),/dimensions))
   
      ind=strsplit(data[1],',', length=len)
     data2=strmid(data,ind,len)
      if(rows gt 1.5) then data2=transpose(data2)
    
    
endif else begin
        data2=strarr(1)
        data2[0]='-9999'
endelse

return, data2
end


  function ReadIndexFile, indexfile
   
     info = File_Info(IndexFile)
     if(info.exists eq 1 and strpos(indexfile,'.ind') gt -0.5) then begin
      OPENR, lun, IndexFile, /GET_LUN
      rows=FILE_LINES(IndexFile)
      IndexPar = STRARR(rows)
      READF, lun, IndexPar;, format='(A30)'
      Free_Lun, lun
     
     row_index=where(stregex(IndexPar,'_Textt0=') gt -0.5) 
     line_index=stregex(IndexPar,'=') &  line_index=line_index[row_index]+1
     interval=strmid(IndexPar[row_index],line_index,99)
         
  
    VectorInd=strarr(16)
     CondInd=strarr(16)
     ValueInd=strarr(16)
     ifInd=strarr(16)
     elseInd=strarr(16)
      
     for i=1,16 do begin
            if (i lt 9.5) then ii=string(i,format='(I1)') else ii=string(i,format='(I2)')
            row_index=where(stregex(IndexPar,'_Tex'+ii+'=') gt -0.5) 
              line_index=stregex(IndexPar,'=') &  line_index=line_index[row_index]+1
              vectorInd[i-1]=strmid(IndexPar[row_index],line_index,99)
            row_index=where(stregex(IndexPar,'_Dropp'+ii+'=') gt -0.5) 
              line_index=stregex(IndexPar,'=') &  line_index=line_index[row_index]+1
              condInd[i-1]=strmid(IndexPar[row_index],line_index,99)
            row_index=where(stregex(IndexPar,'_Text'+ii+'=') gt -0.5) 
              line_index=stregex(IndexPar,'=') &  line_index=line_index[row_index]+1
              ValueInd[i-1]=strmid(IndexPar[row_index],line_index,99)
            row_index=where(stregex(IndexPar,'_Textt'+ii+'=') gt -0.5) 
              line_index=stregex(IndexPar,'=') &  line_index=line_index[row_index]+1
              ifInd[i-1]=strmid(IndexPar[row_index],line_index,99)
            row_index=where(stregex(IndexPar,'_Texttt'+ii+'=') gt -0.5) 
              line_index=stregex(IndexPar,'=') &  line_index=line_index[row_index]+1
              elseInd[i-1]=strmid(IndexPar[row_index],line_index,99)
     endfor
 
 IndValue=strarr(4)
 Values2add=strarr(4)
for i=15,18 do begin
            ii=string(i,format='(I2)')
             row_index=where(stregex(IndexPar,'_Dext'+ii+'=') gt -0.5) 
            line_index=stregex(IndexPar,'=') &  line_index=line_index[row_index]+1
            IndValue[i-15]=strmid(IndexPar[row_index],line_index,99)
         row_index=where(stregex(IndexPar,'_Dextt'+ii+'=') gt -0.5) 
            line_index=stregex(IndexPar,'=') &  line_index=line_index[row_index]+1
          Values2add[i-15]=strmid(IndexPar[row_index],line_index,9999)
   endfor           
            
       
       S1=create_struct('VectorInd',VectorInd,'CondInd',CondInd,'ValueInd',ValueInd,'ifInd',ifInd,'elseInd',elseInd,'IndValue',IndValue,'Values2add',Values2add,'interval',interval)
       
       
        
  endif else S1=create_struct('VectorInd',-9999)
  
  
  return, S1
END




function ReadFloat, file
mistt=systime(1)
 

info = File_Info(File)
if(info.exists eq 1) then begin
      rows=FILE_LINES(file)
     ; rows=rows-488*29
      data = strarr(rows)
      OPENR, lun, file, /GET_LUN
      READF , lun, data,  FORMAT='(A17)'
      Free_Lun, lun
      filt=where(data eq '          -NaN') & if(max(filt) gt -1) then data[filt]='-9.999000E+003'; old format'(E14.6)'
      filt=where(data eq '             -NaN') & if(max(filt) gt -1) then data[filt]='-9.999000000E+003'; 'new format'(E17.9)'
      data2=double(data)
      n_dim=data2[0]
      if(n_dim gt 0.5) then data2=reform(data2[n_dim+1:rows-1],data2[1:n_dim],/overwrite) else data2=data2[2]
endif else begin
data2=-9999
endelse
return, data2
end



Function ReArrange, Data
  mist=SIZE(Data, /DIMENSIONS)
  mist2=SIZE(Data, /N_DIMENSIONS)
  mist3=1
  for i=1,mist2-1 do mist3=mist3*mist[i]
  Data=Reform(data,mist[0],mist3,/overwrite)
  Return, Data
end


pro SaveImage, filename, dataset, colortable
; subroutine to save an 2D-array into a jpg-file colored by a 'colortable'
        dataset=bytscl(dataset)
        dim= size(dataset,/dimensions)
        if colortable gt -0.5 then begin
                DEVICE, DECOMPOSED = 0 
                
                
                loadct,colortable
                 window,24, xsize = dim[0], ysize=dim[1]
                tv, dataset
                ; WSET, 24
                write_jpeg, filename,tvrd(0,true=1), true=1
        endif else begin
        write_jpeg, filename, dataset
        endelse 
end

 
function starttimes, files, ID
  mist=SIZE(Files, /DIMENSIONS)
  n=strlen(ID)
  if(max(mist) gt 2) then files=transpose(files) 
  Year=strmid(Files,strpos(Files,ID)+n-2,4)
  Month=strmid(Files,strpos(Files,ID)+n+3,2)
  Day=strmid(Files,strpos(Files,ID)+n+6,2)
  Hour=strmid(Files,strpos(Files,ID)+n+9,2)
  Minute=strmid(Files,strpos(Files,ID)+n+12,2)
  Second=strmid(Files,strpos(Files,ID)+n+15,2)
  if(max(year) lt 0.1) then t09=0 else t09 = JULDAY(Month, Day, Year, Hour, Minute, Second)-Julday(1,1,2009,0,0,0) ;t09
  return, t09
end
 
 
function str2vec, str
NoNumber=max(STRCMP(max(str), ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'], 1, /FOLD_CASE))
 
 
     vec=float(strsplit(str,',',/extract))
     if( str eq '') then vec = -9999
     if( Nonumber eq 1) then vec = -9999
     
    return, vec
end


function t09, CreationTime 
    ;format1="MM/DD/YYYY, hh:mm:ss AM"
    ;format2="DD-Mmm-YY hh:mm:ss AM
    ;format3: 10/02/2015 13h 54m 29s
    if(strpos(CreationTime,'/') gt -0.5 and strpos(CreationTime,'h') lt -0.5 ) then begin ;format1="MM/DD/YYYY, hh:mm:ss AM"
            hours=float(strmid(CreationTime,strpos(creationtime,':')-2,2))
            if(strpos(creationtime,'PM') gt -0.5) then hours=hours+12
            if(hours eq 12 or hours eq 24) then hours=hours-12 
            minutes=float(strmid(CreationTime,strpos(creationtime,':',/reverse_search)-2,2))
            seconds=float(strmid(CreationTime,strpos(creationtime,':',/reverse_search)+1,2))
            year=float(strmid(CreationTime,strpos(creationtime,'/',/reverse_search)+1,4))
            hilf=strmid(CreationTime,strpos(creationtime,'/',/reverse_search)-2,2)
            if(strpos(hilf,'/') gt -0.5) then hilf=strmid(hilf,1,1)
            day=float(hilf)
            if(strpos(creationtime,'/') ge 2) then month=float(strmid(CreationTime,strpos(creationtime,'/')-2,2))else month=float(strmid(CreationTime,strpos(creationtime,'/')-1,1))
    endif
    if(strpos(CreationTime,'-') gt -0.5) then begin  ;format2="DD-Mmm-YY hh:mm:ss AM
             hours=float(strmid(CreationTime,strpos(creationtime,':')-2,2))
             if(strpos(creationtime,'PM') gt -0.5) then hours=hours+12
             if(hours eq 12 or hours eq 24) then hours=hours-12 
             minutes=float(strmid(CreationTime,strpos(creationtime,':',/reverse_search)-2,2))
             seconds=float(strmid(CreationTime,strpos(creationtime,':',/reverse_search)+1,2))
             year=float(strmid(CreationTime,strpos(creationtime,'-',/reverse_search)+1,2))+2000
             hilf=strmid(CreationTime,strpos(creationtime,'-',/reverse_search)-3,3)
             if(strpos(hilf,'-') gt -0.5) then hilf=strmid(hilf,1,1)
             month=float(strpos('JanFebMarAprMayJunJulAugSepOctNovDec',hilf)/3)+1
             if(strpos(creationtime,'-') ge 2) then day=float(strmid(CreationTime,strpos(creationtime,'-')-2,2))else day=float(strmid(CreationTime,strpos(creationtime,'-')-1,1))
    endif
    if(strpos(CreationTime,'/') gt -0.5 and strpos(CreationTime,'h') gt -0.5 ) then begin ;format3: 10/02/2015 13h 54m 29s
            hours=float(strmid(CreationTime,strpos(creationtime,'h')-2,2))
           ; if(strpos(creationtime,'PM') gt -0.5) then hours=hours+12
          ;  if(hours eq 12 or hours eq 24) then hours=hours-12 
            minutes=float(strmid(CreationTime,strpos(creationtime,'m')-2,2))
            seconds=float(strmid(CreationTime,strpos(creationtime,'s')-2,2))
            year=float(strmid(CreationTime,strpos(creationtime,'/',/reverse_search)+1,4))
            hilf=strmid(CreationTime,strpos(creationtime,'/',/reverse_search)-2,2)
            if(strpos(hilf,'/') gt -0.5) then hilf=strmid(hilf,1,1)
            month=float(hilf)
            if(strpos(creationtime,'/') ge 2) then day=float(strmid(CreationTime,strpos(creationtime,'/')-2,2))else day=float(strmid(CreationTime,strpos(creationtime,'/')-1,1))
    endif
 
    t09 = JULDAY(Month, Day, Year, Hours, Minutes, Seconds)-Julday(1,1,2009,0,0,0)
  
    
    return, max(t09)
end

function t09str, date
  n=max(SIZE(date, /DIMENSIONS))
  
  if(n le 1) then begin
        caldat, date+Julday(1,1,2009,0,0,0),month,day,year,hour,minute,second
        t09=date
  endif else begin
        dt=[0,0,0,0,0,0]
        for i=0,n-1 do dt[i]=date[i]
        year=dt[0]
        month=dt[1]
        day=dt[2]
        hour=dt[3]
        minute=dt[4]
        second=dt[5]
        t09 = JULDAY(Month, Day, Year, Hour, Minute, Second)-Julday(1,1,2009,0,0,0) ;t09
   endelse
  time1=string(Year,format='(I4.4)')+'.'+string(Month,format='(I2.2)')+'.'+string(Day,format='(I2.2)')+'-'+string(Hour,format='(I2.2)')+'h'+string(Minute,format='(I2.2)')+'m'+string(second,format='(I2.2)')+'s'
        
   return, time1
end




function testSCALE, masses,lib,instrument
        ppm=20.0
        scor=0.0
        scorppm=0.0
        accuracy=0.0
        LowDa=0.0015
        if(strpos(instrument,'TOF1000') eq 0) then ppm=60.0
        if(strpos(instrument,'TOF8000') eq 0) then ppm=20.0
        if(strpos(instrument,'TOF1000') eq 0) then LowDa=0.0045
        if(strpos(instrument,'TOF8000') eq 0) then LowDa=0.0015
 
      
        
        
        
       ; masses=m2t(peaktimes,a,t0,ex,SampInt)
        length=max(size(masses,/dimensions))
        deviation=lib[where(abs(lib-masses[0]) eq min(abs(lib-masses[0])))]-masses[0]
      
         MatchLow=0
        if(min(abs(masses-21.0221)) lt LowDa) then MatchLow=MatchLow+1
        if(min(abs(masses-59.0491)) lt LowDa) then MatchLow=MatchLow+1
        if(min(abs(masses-30.994)) lt LowDa) then MatchLow=MatchLow+1
        if(min(abs(masses-33.9935)) lt LowDa) then MatchLow=MatchLow+1
        if(min(abs(masses-47.9966)) lt LowDa) then MatchLow=MatchLow+1
        if(MatchLow lt 1.5) then deviation=9999
       
   
       
       
       
  
        if(deviation lt 9999) then begin
        
        
      
      
      
                split=[masses[0],120.6,250.6,max(masses)]
                for sss=0,2 do begin
                        massi=masses[where(masses gt split[sss] and masses le split[sss+1])]
                        length=max(size(massi,/dimensions))
                        libi=lib[where(lib gt split[sss] and lib le split[sss+1])]
                        for i=0,length-1 do deviation=[deviation,libi[where(abs(libi-massi[i]) eq min(abs(libi-massi[i])))]-massi[i]]
                        if(sss eq 0) then devi120=deviation
                 endfor
         
     
      
      
      
     ;  for i=1,length-1 do deviation=[deviation,lib[where(abs(lib-masses[i]) eq min(abs(lib-masses[i])))]-masses[i]]
     
     
     endif
     
     
     
     
       filter=where( abs(deviation) lt 0.002)
        filter2=where( abs(deviation) lt 0.010)
       filterppm=where( 1e6*abs(deviation)/masses lt ppm)
      
       if(max(filter) gt -0.5) then  scor=float(max(size(masses[filter],/dimensions)))+float(max(size(masses[filter2],/dimensions)))/1000
       if(max(filterppm) gt -0.5) then  scorppm=float(max(size(masses[filterppm],/dimensions)))+0.2- mean(1e6*abs(deviation[filterppm])/masses[filterppm])/100
     
        if(max(filter) gt -0.5) then  accuracy=total(deviation[filter])/scor
 s1=create_struct('scor',scor,'scorppm',scorppm,'accuracy',accuracy, 'deviation',deviation,'masses',masses)
 return, s1
 end    
  

function testSCALE2, massesAll,lib,instrument
    
    

        ppm=20.001
        if(strpos(instrument,'TOF1000') eq 0) then ppm=60.0
        if(strpos(instrument,'TOF8000') eq 0) then ppm=20.0
        if(strpos(instrument,'TOF1000') eq 0) then LowDa=0.0045
        if(strpos(instrument,'TOF8000') eq 0) then LowDa=0.0015
 
          
        scor=0.00001
        scor120=0.00001
       ; massesAll=m2t(peaktimes,a,t0,ex,SampInt,/time)
        massesAll=massesAll[where(massesAll gt 20)]
       
       
       ; reject masses of wich the mass defect is outside the range of the lib compounds
        filter=where((massesAll-floor(massesAll+0.3)) gt massesAll*0.9/800-0.25 ) 
        if(max(filter) gt 0) then massesAll=massesAll[filter] else massesAll=[20,40,60,80,100]
         filter=where((massesAll-floor(massesAll+0.3)) lt massesAll*0.9/800+0.025 ) 
        if(max(filter) gt 0) then massesAll=massesAll[filter] else massesAll=[20,40,60,80,100]
  
        
              
            
       
       if(max(where(2*floor(floor(lib+0.4)/2) eq floor(lib+0.4) and lib gt 50)) lt -0.5)then begin ; remove even masses if mode is H3O+
              filter2=where(massesAll gt 33.9 and massesAll lt 34.1)  ;add m34,and m48 (isotopes of O2 and NO2)
              if(max(filter2) gt 0) then m34=massesAll[filter2]
              filter2=where(massesAll gt 47.9 and massesAll lt 48.1) 
              if(max(filter2) gt 0) then m48=massesAll[filter2]
              filter=where(2*floor(floor(massesAll+0.4)/2) ne floor(massesAll+0.4));selects odd masses
              if(max(filter) gt 0) then massesAll=massesAll[filter] else massesAll=[20,40,60,80,100]
              if(var_exists(m34) gt -0.5) then massesAll=[massesAll,m34]
              if(var_exists(m48) gt -0.5) then massesAll=[massesAll,m48]
       endif
       
  
      
        length=max(size(massesAll,/dimensions))
        
        if (length gt 3000) then begin
          filter=where((massesAll gt  50 and massesAll lt  65) or $
                     (massesAll gt 100 and massesAll lt 115) or $
                     (massesAll gt 150 and massesAll lt 165) or $
                     (massesAll gt 200 and massesAll lt 215) or $
                     (massesAll gt 250 and massesAll lt 265) or $
                     (massesAll gt 300 and massesAll lt 315) or $
                     (massesAll gt 350 and massesAll lt 365) or $
                     (massesAll gt 400 and massesAll lt 415))
         ; filter=where((massesAll gt  330 and massesAll lt  350) or (massesAll gt  106 and massesAll lt  116) or (massesAll gt  190 and massesAll lt  210))
      
          masses=massesAll[filter] 
        endif else masses=massesAll
        
        
        
        deviation=min(abs(lib-masses[0])) 
        if (deviation lt  ppm*1e-6*masses[0]) then scor=scor+1
        devi=0.00001
        MatchLow=0
        if(min(abs(massesAll-21.0221)) lt LowDa) then MatchLow=MatchLow+1
        if(min(abs(massesAll-59.0491)) lt LowDa) then MatchLow=MatchLow+1
        if(min(abs(massesAll-30.994)) lt LowDa) then MatchLow=MatchLow+1
        if(min(abs(massesAll-33.9935)) lt LowDa) then MatchLow=MatchLow+1
        if(min(abs(massesAll-47.9966)) lt LowDa) then MatchLow=MatchLow+1
        if(MatchLow lt 1.5) then deviation=9999
       
   
       
       
        length=max(size(masses,/dimensions))
  
  
  
  
        if(deviation lt 9999) then begin
                split=[20.6,120.6,250.6,max(masses)]
                for sss=0,2 do begin
                        massi=masses[where(masses gt split[sss] and masses le split[sss+1])]
                        length=max(size(massi,/dimensions))
                        libi=lib[where(lib gt split[sss] and lib le split[sss+1])]
                        for i=0,length-1 do begin
                                deviation=min(abs(libi-massi[i]))
                                if (deviation lt   ppm*1e-6*massi[i] or deviation lt 0.0015) then begin 
                                        scor=scor+1
                                        devi =devi+1e4*deviation/massi[i]
                                endif
                        endfor  
                        if(sss eq 0) then scor120=scor
                        if(sss eq 0) then devi120=devi
                 endfor
         
        endif   
        if (scor gt 0.5) then scor=scor+0.2-devi/scor
        if (scor120 gt 0.5) then scor120=scor120+0.2-devi120/scor120
  
  s1=create_struct('scor',scor,'scor120',scor120, 'masses',masses)
 return, s1
 end     
  
 

function testSCALE3, masses,lib,instrument
        masses=masses[where(masses gt 40 and masses lt 250)]
        length=max(size(masses,/dimensions))
       
             deviation=lib[where(abs(lib-masses[0]) eq min(abs(lib-masses[0])))]-masses[0]
       for i=1,length-1 do deviation=[deviation,lib[where(abs(lib-masses[i]) eq min(abs(lib-masses[i])))]-masses[i]]
  
   ppm=50
     if(strpos(instrument,'TOF1000') eq 0) then ppm=150
    if(strpos(instrument,'TOF8000') eq 0) then ppm=50
 
       
         filterppm=where( 1e6*abs(deviation)/masses lt ppm)
      
       if(max(filterppm) gt 0) then  scorppm=float(max(size(masses[filterppm],/dimensions)))+0.2- mean(1e6*abs(deviation[filterppm])/masses[filterppm])/100
     
        
    ;return, 1000*median(deviation
    if(var_exists(scorppm) eq -1) then scorppm=0
   return, 1000-scorppm
 end    


function var_exists,variable
        value=-1
        help, variable, output=dummy
        if(STRPOS( dummy[0], 'Undefined' ) lt -0.5) then value=1
        return, value
end
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;;;;;;;
;;;;;;;                   Other routines
;;;;;;;
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO ____________other_routines
 end
 


    
function quantile_int, x, p, type
   ;;
   ;; The workhorse routine. The following is the relevant
   ;; documentation of the quantile function from R - note that this
   ;; is not here to explain functionality but to give background
   ;; information about the possible types of quantiles that can be
   ;; calculated. 
   ;;
   ;; quantile                package:stats                R Documentation
   ;; Sample Quantiles
   ;;
   ;; Description:
   ;;
   ;;     The generic function 'quantile' produces sample quantiles
   ;;     corresponding to the given probabilities. The smallest observation
   ;;     corresponds to a probability of 0 and the largest to a probability
   ;;     of 1.
   ;;
   ;; Usage:
   ;;
   ;;     quantile(x, ...)
   ;;
   ;;     ## Default S3 method:
   ;;     quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
   ;;              names = TRUE, type = 7, ...)
   ;;
   ;; Arguments:
   ;;
   ;;       x: numeric vectors whose sample quantiles are wanted.  Missing
   ;;          values are ignored.
   ;;
   ;;   probs: numeric vector of probabilities with values in [0,1].
   ;;
   ;;   na.rm: logical; if true, any 'NA' and 'NaN''s are removed from 'x'
   ;;          before the quantiles are computed.
   ;;
   ;;   names: logical; if true, the result has a 'names' attribute.  Set to
   ;;          'FALSE' for speedup with many 'probs'.
   ;;
   ;;    type: an integer between 1 and 9 selecting one of the nine quantile
   ;;          algorithms detailed below to be used.
   ;;
   ;;     ...: further arguments passed to or from other methods.
   ;;
   ;; Details:
   ;;
   ;;     A vector of length 'length(probs)' is returned; if 'names = TRUE',
   ;;     it has a 'names' attribute.
   ;;
   ;;     'NA' and 'NaN' values in 'probs' are propagated to the result.
   ;;
   ;; Types:
   ;;
   ;;     'quantile' returns estimates of underlying distribution quantiles
   ;;     based on one or two order statistics from the supplied elements in
   ;;     'x' at probabilities in 'probs'.  One of the nine quantile
   ;;     algorithms discussed in Hyndman and Fan (1996), selected by
   ;;     'type', is employed.
   ;;
   ;;     Sample quantiles of type i are defined by
   ;;
   ;;              Q[i](p) = (1 - gamma) x[j] + gamma x[j+1],
   ;;
   ;;     where 1 <= i <= 9, (j-m)/n <= p < (j-m+1)/ n, x[j] is the jth
   ;;     order statistic, n is the sample size, and m is a constant
   ;;     determined by the sample quantile type. Here gamma depends on the
   ;;     fractional part of g = np+m-j.
   ;;
   ;;     For the continuous sample quantile types (4 through 9), the sample
   ;;     quantiles can be obtained by linear interpolation between the kth
   ;;     order statistic and p(k):
   ;;
   ;;             p(k) = (k - alpha) / (n - alpha - beta + 1),
   ;;
   ;;     where alpha and beta are constants determined by the type.
   ;;     Further, m = alpha + p(1 - alpha - beta), and gamma = g.
   ;;
   ;;     *Discontinuous sample quantile types 1, 2, and 3*
   ;;
   ;;
   ;;     Type 1 Inverse of empirical distribution function.
   ;;
   ;;     Type 2 Similar to type 1 but with averaging at discontinuities.
   ;;
   ;;     Type 3 SAS definition: nearest even order statistic.
   ;;
   ;;     *Continuous sample quantile types 4 through 9*
   ;;
   ;;
   ;;     Type 4 p(k) = k / n. That is, linear interpolation of the
   ;;          empirical cdf.
   ;;
   ;;
   ;;     Type 5 p(k) = (k - 0.5) / n. That is a piecewise linear function
   ;;          where the knots are the values midway through the steps of
   ;;          the empirical cdf. This is popular amongst hydrologists.
   ;;
   ;;
   ;;     Type 6 p(k) = k / (n + 1). Thus p(k) = E[F(x[k])]. This is used by
   ;;          Minitab and by SPSS.
   ;;
   ;;
   ;;     Type 7 p(k) = (k - 1) / (n - 1). In this case, p(k) =
   ;;          mode[F(x[k])]. This is used by S.
   ;;
   ;;
   ;;     Type 8 p(k) = (k - 1/3) / (n + 1/3). Then p(k) =~ median[F(x[k])].
   ;;          The resulting quantile estimates are approximately
   ;;          median-unbiased regardless of the distribution of 'x'.
   ;;
   ;;
   ;;     Type 9 p(k) = (k - 3/8) / (n + 1/4). The resulting quantile
   ;;          estimates are approximately unbiased for the expected order
   ;;          statistics if 'x' is normally distributed.
   ;;
   ;;
   ;;     Hyndman and Fan (1996) recommend type 8. The default method is
   ;;     type 7, as used by S and by R < 2.0.0.
   ;;
   ;; Author(s):
   ;;
   ;;     of the version used in R >= 2.0.0, Ivan Frohne and Rob J Hyndman.
   ;;
   ;; References:
   ;;
   ;;     Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) _The New S
   ;;     Language_. Wadsworth & Brooks/Cole.
   ;;
   ;;     Hyndman, R. J. and Fan, Y. (1996) Sample quantiles in statistical
   ;;     packages, _American Statistician_, *50*, 361-365.
   ;;
   ;; See Also:
   ;;
   ;;     'ecdf' for empirical distributions of which 'quantile' is an
   ;;     inverse; 'boxplot.stats' and 'fivenum' for computing other
   ;;     versions of quartiles, etc.
   ;;
   ;; Examples:
   ;;
   ;;     quantile(x <- rnorm(1001))# Extremes & Quartiles by default
   ;;     quantile(x,  probs=c(.1,.5,1,2,5,10,50, NA)/100)
   ;;
   ;;     ### Compare different types
   ;;     p <- c(0.1,0.5,1,2,5,10,50)/100
   ;;     res <- matrix(as.numeric(NA), 9, 7)
   ;;     for(type in 1:9) res[type, ] <- y <- quantile(x,  p, type=type)
   ;;     dimnames(res) <- list(1:9, names(y))
   ;;     round(res, 3)


   

   common machine_precision, eps, epsneg, xmin, xmax

   if (n_elements(eps) eq 0) Then begin
      mtmp = machar(/double)
      eps = mtmp.eps
      epsneg = mtmp.epsneg
      xmin = mtmp.xmin
      xmax = mtmp.xmax
   endif

   n_x = n_elements(x)

   if (type le 3) then begin
      ;;------------------------
      ;; Discrete estimators
      ;;------------------------

      ;; The m value
      m_types = [0, 0, -0.5]
      m = m_types[type-1]

      ;; Find the j value.
      j = floor(p*n_x + m)
      ;; Find g
      g = p*n_x + m - j

      ;; determine gamma.
      case type of
         1: if (g gt 0) then gamma = 1.0 else gamma = 0.0
         2: if (g gt 0) Then gamma = 1.0 else gamma = 0.5
         3: begin
            if (g gt 0) Then gamma = 1.0 $
            else begin
               if ((j mod 2) eq 0) then gamma = 0.0 else gamma = 1.0
            endelse
         end
      endcase

   endif else begin
      ;;-------------------------
      ;; Continuous estimators
      ;;-------------------------
      
      alpha_all = [0, 0.5, 0., 1, 0.333333333, 0.375]
      alpha = alpha_all[type-4.]
      beta = alpha

      m = alpha+p*(1.0-alpha-beta)
      j = floor(p*n_x + m + 4.0*eps)
      g = p*n_x+m - j
      if (abs(g) lt 4*eps) then gamma = 0.0
      gamma = g


   endelse

   j = j-1

   Q_p = (1.0-gamma)*x[j > 0]+gamma*x[j+1 < (n_x-1)]


;   ii = findgen(n_elements(xs))/(n_elements(xs)-1.0)
;   
;   for i = 0,N_elements(p)-1 do begin
;      res[i] = lin_interp_solve(xs, ii, p[i])
;   endfor;

   return, Q_p

end



function quantile, x, p, type = type, sorted = sorted, $
                   nan=nan
;+
; NAME: QUANTILE
;
; PURPOSE: Calculate the quantiles of a distribution of points
; 
; CATEGORY: Statistics, R emulation
;
; CALLING SEQUENCE: 
;       res = QUANTILE(X, P [KEYWORDS=KEYWORDS])
;
; INPUTS: 
;
;     X: The data. This should be a numerical vector.
;     P: The quantile levels requested. This can be a scalar or a
;        vector with values beteen 0 and 1.
; 
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS: 
;
;             NAN: Filter out NaN
;          SORTED: If the data is already sorted, set this keyword to
;                  avoid redoing the sort.
;            TYPE: The type of quantile calculation. See the
;                  documentation of the R function quantile for
;                  details (or look at the source code for
;                  quantile_int which contains a copy of this
;                  documentation) 
; 
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
;    x = randomn(sss, 1000)
;    print, quantile(x, [0.025, 0.16, 0.5, 0.84, 0.975])
;
; This should print something close to -2, -1, 0, 1, 2
;    
;
; MODIFICATION HISTORY:
;
;       Oct 22, 2008, Jarle Brinchmann (jarle@astro.up.pt)
;          Documented routine
;
;-

   
   ;;
   ;; Calculate quantiles of a distribution
   ;; 
   ;; Extended to work as R's quantile routine.
   ;;
   if (n_Elements(type) eq 0) Then type = 8
   if (type lt 1 or type gt 9) Then begin
      print, 'QUANTILE: Only type 1-9 is supported!'
      return, -1
   endif

   ;;-----------------------------------------------------------------
   ;; XS is X_(i) in the statistical lingo - ie. the order statistic
   ;; of X
   ;;-----------------------------------------------------------------

   n_p = n_elements(p)
   if (size(p, /n_dim) eq 0) then is_scalar = 1 else is_scalar = 0
   res = fltarr(n_p)

   if (keyword_set(sorted)) then xs = x $
   else $
    xs = x[sort(x)]

   if (keyword_set(nan)) then begin
      bad = where(finite(x) eq 0, n_bad, compl=good)
      if (n_bad eq n_elements(xs)) then begin
         res[*] = !values.f_nan
         if (is_scalar) then return ,res[0] else return, res
      endif
      tmp_x = x[good]
      si = sort(tmp_x)
      xs = tmp_x[si]
      xs = xs[good]
   endif 



   for i = 0L, n_p-1 do $
           res[i] = quantile_int(xs, p[i], type)
   
   if (is_scalar) then return ,res[0] else return, res

end

FUNCTION evenodd,num
; simple function, returns 0 if a number is even, 1 if it is odd
; if not given an integer, rounds it and lets you know if rounded
; number is even or odd.  Zero returns as 'even'
  fnum=round(num)
  if (2*(fnum/2) eq fnum) then begin
      return,0
  end else begin
      return,1
  end
  
END



 
 
 
 
 
 
 

 
 
  