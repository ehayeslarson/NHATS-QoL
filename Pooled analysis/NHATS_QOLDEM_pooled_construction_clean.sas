********************************************************************************************************;
*                      GENERAL INFORMATION SECTION                                           			;
*Study: Racial and ethnic differences in quality of life among older adults with dementia in NHATS		;			 			 		
*Principal Investigator: Dr. Elizabeth Rose Mayeda                                                 		;
*Code created by: Taylor Mobley 	                       								     			; 	
*Purpose:   Create pooled data set with NHAT Rounds 1-8. Use QOL_DEM_analysis as template		       	;
*Input: 	NHATS_Round_1_SP_File_v2, NHATS_Round_2_SP_File_v2, NHATS_Round_3_SP_File_v2				;
*			NHATS_Round_4_SP_File_v2, NHATS_Round_5_SP_File_v2, NHATS_Round_6_SP_File_v2				;
*			NHATS_Round_7_SP_File_v2, NHATS_Round_8_SP_File_v2											;
*Output: 																								;                                        
*Request:   												 											;
********************************************************************************************************;

Libname NHATS 'C:\Users\tmobley\Box\NHATS\DATA\raw_data';
Libname QoL 'C:\Users\tmobley\Box\NHATS\DATA\analysis_datasets'; 
options fmtsearch=(NHATS) nofmterr;

** FORMATS FOR CONSTRUCTED VARIABLES **;
Proc format library=NHATS;
	value r1demclas
	1="1:Probable"
	2="2:Possible"
	3="3:No Impairment"
	-1="-1:NH resident"
	-9="-9:Missing"
	;
	value r1ad8dem
	1="1:Probable"
	2="2:Possible"
	;
	value r1clockf
	0-1="0-1:Impaired"
	2-5="2-5:Not impaired"
	;
	value r1wordrecf
	0-3="0-3: Impaired "
	4-20="4-20: Not impaired "
	;
	value r1dateprf
	0-3="0-3: Impaired "
	4-8="4-8: Not impaired "
	;
*Rounds 2 and following;
	value demclas
	1="1:Probable dementia"
	2="2:Possible dementia"
	3="3:No dementia"
	-1="-1:Deceased or nursing home resident"
	-9="-9:Missing"
	;
	value ad8dem
	1="1:Meets dementia criteria"
	2="2:Does not meet dementia criteria"
	;
	value clockf
	0-1="0-1:Impaired"
	2-5="2-5:Not impaired"
	;
	value wordrecf
	0-3="0-3: Impaired "
	4-20="4-20: Not impaired "
	;
	value dateprf
	0-3="0-3: Impaired "
	4-8="4-8: Not impaired "
	;
*Metropolitan/non-metropolitan derived var;
	VALUE W00metro
	   -1 = '-1 Inapplicable'	
		1 = '1 Metropolitan'
		2 = '2 Non-metropolitan'
	;
	Run;

/*Import NHATS Rounds 1-8*/
Data NHATS_R1; Set NHATS.NHATS_Round_1_SP_File; Run;		/*8245 obs*/
Data NHATS_R2; Set NHATS.NHATS_Round_2_SP_File_v2; Run;		/*7075 obs*/
Data NHATS_R3; Set NHATS.NHATS_Round_3_SP_File; Run;		/*5799 obs*/
Data NHATS_R4; Set NHATS.NHATS_Round_4_SP_File; Run;		/*4737 obs*/
Data NHATS_R5; Set NHATS.NHATS_Round_5_SP_File_v2; Run;		/*8334 obs*/
Data NHATS_R6; Set NHATS.NHATS_Round_6_SP_File_v2; Run;		/*7276 obs*/
Data NHATS_R7; Set NHATS.NHATS_Round_7_SP_File; Run;		/*6312 obs*/
Data NHATS_R8; Set NHATS.NHATS_Round_8_SP_File; Run;		/*5547 obs*/

/************************************/
/*** Construct prob/poss dementia ***/
/************************************/

	/** DEMENTIA CLASSIFICATION VARIABLE - ROUND 1 **
	** NOTE: The input file to run this code is the NHATS_Round_1_File**
	** DATE STEP CODE FOR CREATING DEMENTIA CLASSIFICATION VARIABLE **/
	Data demclas_R1; Set NHATS_R1;
	length r1demclas
	ad8_dem ad8_1-ad8_8 ad8miss_1-ad8miss_8 ad8_score ad8_miss
	date_mon date_day date_yr date_dow date_sum date_sumr
	preslast presfirst vplast vpfirst presvp presvpr date_prvp
	clock_scorer irecall drecall wordrecall0_20
	clock65 word65 datena65 domain65
	3;
	label r1demclas="R1 NHATS Dementia Classification 65+";
	** 1) SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS) **;
	if r1dresid=3 then r1demclas=-9 ;
	if r1dresid=4 then r1demclas=-1 ;
	** 2) CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY **;
	if hc1disescn9=1 and is1resptype in (1,2) then r1demclas=1 ;
	** 3a) CODE AD8_SCORE **;
	array think {*} cp1chgthink1-cp1chgthink8; ** QUESTIONNAIRE ITEMS **;
	array ad8item {*} ad8_1-ad8_8;
	array ad8miss {*} ad8miss_1-ad8miss_8;
	ad8_score =-1;
	ad8_miss =-1;
	do i=1 to dim(ad8item);
	** INITIALIZE COUNTS TO NOT APPLICABLE**;
	ad8item{i}=-1;
	ad8miss{i}=-1;
	** ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY REPORTED DIAGNOSIS **;
	if is1resptype=2 and r1demclas=. then do;
	ad8item{i}=.;
	if think{i} in (1,3) then ad8item{i}=1; ** PROXY REPORTS A CHANGE OR ALZ/DEMENTIA **;
	else if think{i}=2 then ad8item{i}=0; ** PROXY REPORTS NO CHANGE **;
	ad8_score=sum(of ad8item{*}); ** COUNT AD8 ITEMS **;
	if ad8item{i} in (0,1) then ad8miss{i}=0;
	else if ad8item{i}=. then ad8miss{i}=1;
	ad8_miss=sum(of ad8miss{*}); ** COUNT MISSING AD8 ITEMS **;
	end;
	end;
	** 3b) CODE AD8 DEMENTIA CLASS **;
	if ad8_score>=2 then ad8_dem=1 ; ** IF SCORE >=2 THEN MEETS AD8 CRITERION **;
	if ad8_score in (0,1) or ad8_miss=8 then ad8_dem=2; ** IF SCORE IS 0 OR 1 OR ALL ITEMS MISSING THEN DOES NOT MEET AD8 CRITERION **;
	** 4) UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS **;
	if r1demclas=. then do;
	if ad8_dem=1 then r1demclas=1; ** PROBABLE BASED ON AD8 SCORE **;
	if ad8_dem=2 and cg1speaktosp=2 then r1demclas=3; ** NO DIAGNOSIS, DOES NOT MEET AD8 CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS **;
	end;
	** 5) CODE DATE ITEMS AND COUNT **;
	array cg1date {*} cg1todaydat1-cg1todaydat4;
	array date_item {*} date_mon date_day date_yr date_dow;
	do i=1 to dim(date_item);
	if cg1date{i} > 0 then date_item{i}=cg1date{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING **;
	if cg1date{i} in (-7,2) then date_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF **;
	date_sum=sum(of date_item{*}); ** COUNT CORRECT DATE ITEMS **;
	end;
	if date_sum=. then do;
	if cg1speaktosp=2 then date_sum=-2; ** PROXY SAYS CAN'T SPEAK TO SP **;
	else if cg1speaktosp=1 and max(of cg1date{*})=-1 then date_sum=-3; ** PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER **;
	end;
	date_sumr=date_sum;
	if date_sum=-2 then date_sumr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if date_sum=-3 then date_sumr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 6) PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT **;
	array cg1pres {*} cg1presidna1 cg1presidna3 cg1vpname1 cg1vpname3;
	array pres_item {*} preslast presfirst vplast vpfirst;
	do i=1 to dim(pres_item);
	if cg1pres{i} > 0 then pres_item{i}=cg1pres{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES -1,-9 LEFT MISSING **;
	if cg1pres{i} in (-7,2) then pres_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF **;
	presvp=sum(of pres_item{*}); ** COUNT CORRECT PRESIDENT/VEEP NAME ITEMS **;
	end;
	if presvp=. then do;
	if cg1speaktosp=2 then presvp=-2; ** PROXY SAYS CAN'T SPEAK TO SP **;
	else if cg1speaktosp=1 and max(of cg1pres{*})=-1 then presvp=-3; ** PROXY SAYS CAN SPEAK TO SP BUT SP UNABLE TO ANSWER **;
	end;
	presvpr=presvp;
	if presvp=-2 then presvpr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if presvp=-3 then presvpr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 7) ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING **;
	date_prvp=sum(date_sumr,presvpr);
	** 8) EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE **;
	clock_scorer=cg1dclkdraw;
	if cg1dclkdraw in (-2,-9) then clock_scorer=.;
	if cg1dclkdraw in (-3,-4,-7) then clock_scorer=0;
	** IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK **;
	if cg1dclkdraw=-9 and cg1speaktosp=1 then clock_scorer=2; ** IF PROXY SAID CAN ASK SP **;
	if cg1dclkdraw=-9 and cg1speaktosp=-1 then clock_scorer=3; ** IF SELF RESPONDENT **;
	** 9) MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL **;
	array cg1recall {*} cg1dwrdimmrc cg1dwrddlyrc;
	array word_recall {*} irecall drecall;
	do i=1 to dim(word_recall);
	word_recall{i}=cg1recall{i};
	if cg1recall{i} in (-2,-1) then word_recall{i}=.;
	if cg1recall{i} in (-7,-3) then word_recall{i}=0;
	wordrecall0_20=sum(of word_recall{*});
	end;
	** 10) CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE **;
	** I.E. PROXY BUT PROXY SAYS CAN ASK SP, NOT FQ ONLY, NOT NH **;
	if 1 < clock_scorer <=5 then clock65=0;
	if 0 <=clock_scorer <=1 then clock65=1;
	if 3 < wordrecall0_20 <=20 then word65=0;
	if 0 <= wordrecall0_20 <=3 then word65=1;
	if 3 < date_prvp <= 8 then datena65=0;
	if 0 <= date_prvp <= 3 then datena65=1;
	** 10) CREATE COGNITIVE DOMAIN SCORE **;
	array domains {*} clock65 word65 datena65;
	do i=1 to dim(domains);
	domain65=sum(of domains{*});
	end;
	** 11) UPDATE COGNITIVE CLASSIFICATION **;
	if r1demclas=. and cg1speaktosp in (-1,1) then do;
	if 2 <= domain65 <=3 then r1demclas=1; ** PROBABLE DEMENTIA **;
	if domain65 =1 then r1demclas=2; ** POSSIBLE DEMENTIA **;
	if domain65 =0 then r1demclas=3; ** NO DEMENTIA **;
	format r1demclas r1demclas. ad8_dem r1ad8dem. clock65 r1clockf. word65 r1wordrecf. datena65 r1dateprf.;
	end;
	Run;

		/*Rename vars from prob/poss dementia algorithm to indicate R1*/
		Data demclas_R1; Set demclas_R1;
			Rename ad8_dem=r1ad8_dem ad8_1=r1ad8_1 ad8_2=r1ad8_2 ad8_3=r1ad8_3 ad8_4=r1ad8_4 
					ad8_5=r1ad8_5 ad8_6=r1ad8_6 ad8_7=r1ad8_7 ad8_8=r1ad8_8
					ad8miss_1=r1ad8miss_1 ad8miss_2=r1ad8miss_2 ad8miss_3=r1ad8miss_3 ad8miss_4=r1ad8miss_4
					ad8miss_5=r1ad8miss_5 ad8miss_6=r1ad8miss_6 ad8miss_7=r1ad8miss_7 ad8miss_8=r1ad8miss_8 
					ad8_score=r1ad8_score ad8_miss=r1ad8_miss date_mon=r1date_mon date_day=r1date_day 
					date_yr=r1date_yr date_dow=r1date_dow date_sum=r1date_sum date_sumr=r1date_sumr
					preslast=r1preslast presfirst=r1presfirst vplast=r1vplast vpfirst=r1vpfirst presvp=r1presvp 
					presvpr=r1presvpr date_prvp=r1date_prvp clock_scorer=r1clock_scorer irecall=r1irecall 
					drecall=r1drecall wordrecall0_20=r1wordrecall0_20
					clock65=r1clock65 word65=r1word65 datena65=r1datena65 domain65=r1domain65; Run;

	/** DEMENTIA CLASSIFICATION VARIABLE - ROUND 2 **
	** NOTE: The input file to run this code is the NHATS_Round_2_SP_File**
	** DATE STEP CODE FOR CREATING DEMENTIA CLASSIFICATION VARIABLE **/
	Data demclas_R2; Set NHATS_R2;
	length r2demclas
	r2ad8_dem r2ad8_1-r2ad8_8 r2ad8miss_1-r2ad8miss_8 r2ad8_score r2ad8_miss
	r2date_mon r2date_day r2date_yr r2date_dow r2date_sum r2date_sumr
	r2preslast r2presfirst r2vplast r2vpfirst r2presvp r2presvpr r2date_prvp
	r2clock_scorer r2irecall r2drecall r2wordrecall0_20
	r2clock65 r2word65 r2datena65 r2domain65
	3;
	label r2demclas="R2 NHATS Dementia Classification 65+";
	if cg2dwrdimmrc = 10 and cg2dwrddlyrc = -3 then cg2dwrdimmrc = -3; **USE THIS LINE TO FIX A CODING ERROR IN ROUND 2 ONLY**;
	** 1) SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED) **;
	if r2dresid=7 then r2demclas=-9;
	if r2dresid in (6,8) then r2demclas=-1;
	** 2) CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY **;
	if hc2disescn9 in (1,7) and is2resptype in (1,2) then r2demclas=1;
	** 3a) CODE AD8_SCORE **;
	array r2think {*} cp2chgthink1-cp2chgthink8; ** QUESTIONNAIRE ITEMS **;
	array r2ad8item {*} r2ad8_1-r2ad8_8;
	array r2ad8miss {*} r2ad8miss_1-r2ad8miss_8;
	r2ad8_score =-1;
	r2ad8_miss =-1;
	do i=1 to dim(r2ad8item);
	** INITIALIZE COUNTS TO NOT APPLICABLE**;
	r2ad8item{i}=-1;
	r2ad8miss{i}=-1;
	** ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY
	REPORTED DIAGNOSIS **;
	if is2resptype=2 and r2demclas=. then do;
	r2ad8item{i}=.;
	if r2think{i} in (1,3) then r2ad8item{i}=1; ** PROXY REPORTS A CHANGE OR ALZ/DEMENTIA **;
	else if r2think{i}=2 then r2ad8item{i}=0; ** PROXY REPORTS NO CHANGE **;
	r2ad8_score=sum(of r2ad8item{*}); ** COUNT AD8 ITEMS **;
	if r2ad8item{i} in (0,1) then r2ad8miss{i}=0;
	else if r2ad8item{i}=. then r2ad8miss{i}=1;
	r2ad8_miss=sum(of r2ad8miss{*}); ** COUNT MISSING AD8 ITEMS **;
	end;
	end;
	** 3b) CODE AD8 DEMENTIA CLASS **;
	if cp2dad8dem=1 and is2resptype=2 and r2demclas=. then r2ad8_score=8;
	if r2ad8_score>=2 then r2ad8_dem=1 ; ** IF SCORE >=2 THEN MEETS AD8 CRITERION **;
	if r2ad8_score in (0,1) or (r2ad8_miss=8 and r2ad8_dem=.) then r2ad8_dem=2; ** IF SCORE IS 0 OR 1 OR ALL ITEMS MISSING
	THEN DOES NOT MEET AD8 CRITERION **;
	** 4) UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS **;
	if r2demclas=. then do;
	if r2ad8_dem=1 then r2demclas=1; ** PROBABLE BASED ON AD8 SCORE **;
	if r2ad8_dem=2 and cg2speaktosp=2 then r2demclas=3; ** NO DIAGNOSIS, DOES NOT MEET AD8
	CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS **;
	end;
	** 5) CODE DATE ITEMS AND COUNT **;
	array cg2date {*} cg2todaydat1-cg2todaydat4; **USE THIS LINE FOR ROUNDS 1-3, 5**;
		/*array cg4date {*} cg4todaydat1 cg4todaydat2 cg4todaydat3 cg4todaydat5; **USE THIS LINE FOR ROUND 4***/
	array r2date_item {*} r2date_mon r2date_day r2date_yr r2date_dow;
	do i=1 to dim(r2date_item);
	if cg2date{i} > 0 then r2date_item{i}=cg2date{i}; *** CODE ONLY YES/NO RESPONSES: MISSING/N.A.
	CODES -1,-9 LEFT MISSING **;
	if cg2date{i} in (-7,2) then r2date_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r2date_sum=sum(of r2date_item{*}); ** COUNT CORRECT DATE ITEMS
	**;
	end;
	if r2date_sum=. then do;
	if cg2speaktosp=2 then r2date_sum=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg2speaktosp=1 and max(of cg2date{*})=-1 then r2date_sum=-3; ** PROXY SAYS CAN SPEAK TO
	SP BUT SP UNABLE TO ANSWER **;
	end;
	r2date_sumr=r2date_sum;
	if r2date_sum=-2 then r2date_sumr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if r2date_sum=-3 then r2date_sumr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 6) PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT **;
	array cg2pres {*} cg2presidna1 cg2presidna3 cg2vpname1 cg2vpname3;
	array r2pres_item {*} r2preslast r2presfirst r2vplast r2vpfirst;
	do i=1 to dim(r2pres_item);
	if cg2pres{i} > 0 then r2pres_item{i}=cg2pres{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES
	-1,-9 LEFT MISSING **;
	if cg2pres{i} in (-7,2) then r2pres_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r2presvp=sum(of r2pres_item{*}); ** COUNT CORRECT PRESIDENT/VEEP NAME ITEMS
	**;
	end;
	if r2presvp=. then do;
	if cg2speaktosp=2 then r2presvp=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg2speaktosp=1 and max(of cg2pres{*})=-1 then r2presvp=-3; ** PROXY SAYS CAN SPEAK TO SP
	BUT SP UNABLE TO ANSWER **;
	end;
	r2presvpr=r2presvp;
	if r2presvp=-2 then r2presvpr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if r2presvp=-3 then r2presvpr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 7) ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING **;
	r2date_prvp=sum(r2date_sumr,r2presvpr);
	** 8) EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE **;
	r2clock_scorer=cg2dclkdraw;
	if cg2dclkdraw in (-2,-9) then r2clock_scorer=.;
	if cg2dclkdraw in (-3,-4,-7) then r2clock_scorer=0;
	** IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK **;
	if cg2dclkdraw=-9 and cg2speaktosp=1 then r2clock_scorer=2; ** IF PROXY SAID CAN ASK SP **;
	if cg2dclkdraw=-9 and cg2speaktosp=-1 then r2clock_scorer=3; ** IF SELF RESPONDENT **;
	** 9) MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL **;
	array cg2recall {*} cg2dwrdimmrc cg2dwrddlyrc;
	array r2word_recall {*} r2irecall r2drecall;
	do i=1 to dim(r2word_recall);
	r2word_recall{i}=cg2recall{i};
	if cg2recall{i} in (-2,-1) then r2word_recall{i}=.;
	if cg2recall{i} in (-7,-3) then r2word_recall{i}=0;
	r2wordrecall0_20=sum(of r2word_recall{*});
	end;
	** 10) CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE **;
	** I.E. PROXY BUT PROXY SAYS CAN ASK SP, NOT FQ ONLY, NOT NH **;
	if 1 < r2clock_scorer <=5 then r2clock65=0;
	if 0 <= r2clock_scorer <=1 then r2clock65=1;
	if 3 < r2wordrecall0_20 <=20 then r2word65=0;
	if 0 <= r2wordrecall0_20 <=3 then r2word65=1;
	if 3 < r2date_prvp <= 8 then r2datena65=0;
	if 0 <= r2date_prvp <= 3 then r2datena65=1;
	** 10) CREATE COGNITIVE DOMAIN SCORE **;
	array r2domains {*} r2clock65 r2word65 r2datena65;
	do i=1 to dim(r2domains);
	r2domain65=sum(of r2domains{*});
	end;
	** 11) UPDATE COGNITIVE CLASSIFICATION **;
	if r2demclas=. and cg2speaktosp in (-1,1) then do;
	if 2 <= r2domain65 <=3 then r2demclas=1; ** PROBABLE DEMENTIA **;
	if r2domain65 =1 then r2demclas=2; ** POSSIBLE DEMENTIA **;
	if r2domain65 =0 then r2demclas=3; ** NO DEMENTIA **;
	end;
	format r2demclas demclas. r2ad8_dem ad8dem. r2clock65 clockf. r2word65 wordrecf. r2datena65 dateprf.;
	Run;


	/** DEMENTIA CLASSIFICATION VARIABLE - ROUND 3 **
	** NOTE: The input file to run this code is the NHATS_Round_2_SP_File**
	** DATE STEP CODE FOR CREATING DEMENTIA CLASSIFICATION VARIABLE **/
	Data demclas_R3; Set NHATS_R3;
	length r3demclas
	r3ad8_dem r3ad8_1-r3ad8_8 r3ad8miss_1-r3ad8miss_8 r3ad8_score r3ad8_miss
	r3date_mon r3date_day r3date_yr r3date_dow r3date_sum r3date_sumr
	r3preslast r3presfirst r3vplast r3vpfirst r3presvp r3presvpr r3date_prvp
	r3clock_scorer r3irecall r3drecall r3wordrecall0_20
	r3clock65 r3word65 r3datena65 r3domain65
	3;
	label r3demclas="R3 NHATS Dementia Classification 65+";
		/*if cg2dwrdimmrc = 10 and cg2dwrddlyrc = -3 then cg2dwrdimmrc = -3; **USE THIS LINE TO FIX A CODING ERROR IN ROUND 2 ONLY***/
	** 1) SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED) **;
	if r3dresid=7 then r3demclas=-9;
	if r3dresid in (6,8) then r3demclas=-1;
	** 2) CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY **;
	if hc3disescn9 in (1,7) and is3resptype in (1,2) then r3demclas=1;
	** 3a) CODE AD8_SCORE **;
	array r3think {*} cp3chgthink1-cp3chgthink8; ** QUESTIONNAIRE ITEMS **;
	array r3ad8item {*} r3ad8_1-r3ad8_8;
	array r3ad8miss {*} r3ad8miss_1-r3ad8miss_8;
	r3ad8_score =-1;
	r3ad8_miss =-1;
	do i=1 to dim(r3ad8item);
	** INITIALIZE COUNTS TO NOT APPLICABLE**;
	r3ad8item{i}=-1;
	r3ad8miss{i}=-1;
	** ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY
	REPORTED DIAGNOSIS **;
	if is3resptype=2 and r3demclas=. then do;
	r3ad8item{i}=.;
	if r3think{i} in (1,3) then r3ad8item{i}=1; ** PROXY REPORTS A CHANGE OR ALZ/DEMENTIA **;
	else if r3think{i}=2 then r3ad8item{i}=0; ** PROXY REPORTS NO CHANGE **;
	r3ad8_score=sum(of r3ad8item{*}); ** COUNT AD8 ITEMS **;
	if r3ad8item{i} in (0,1) then r3ad8miss{i}=0;
	else if r3ad8item{i}=. then r3ad8miss{i}=1;
	r3ad8_miss=sum(of r3ad8miss{*}); ** COUNT MISSING AD8 ITEMS **;
	end;
	end;
	** 3b) CODE AD8 DEMENTIA CLASS **;
	if cp3dad8dem=1 and is3resptype=2 and r3demclas=. then r3ad8_score=8;
	if r3ad8_score>=2 then r3ad8_dem=1 ; ** IF SCORE >=2 THEN MEETS AD8 CRITERION **;
	if r3ad8_score in (0,1) or (r3ad8_miss=8 and r3ad8_dem=.) then r3ad8_dem=2; ** IF SCORE IS 0 OR 1 OR ALL ITEMS MISSING
	THEN DOES NOT MEET AD8 CRITERION **;
	** 4) UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS **;
	if r3demclas=. then do;
	if r3ad8_dem=1 then r3demclas=1; ** PROBABLE BASED ON AD8 SCORE **;
	if r3ad8_dem=2 and cg3speaktosp=2 then r3demclas=3; ** NO DIAGNOSIS, DOES NOT MEET AD8
	CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS **;
	end;
	** 5) CODE DATE ITEMS AND COUNT **;
	array cg3date {*} cg3todaydat1-cg3todaydat4; **USE THIS LINE FOR ROUNDS 1-3, 5**;
		/*array cg4date {*} cg4todaydat1 cg4todaydat2 cg4todaydat3 cg4todaydat5; **USE THIS LINE FOR ROUND 4*/
	array r3date_item {*} r3date_mon r3date_day r3date_yr r3date_dow;
	do i=1 to dim(r3date_item);
	if cg3date{i} > 0 then r3date_item{i}=cg3date{i}; *** CODE ONLY YES/NO RESPONSES: MISSING/N.A.
	CODES -1,-9 LEFT MISSING **;
	if cg3date{i} in (-7,2) then r3date_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r3date_sum=sum(of r3date_item{*}); ** COUNT CORRECT DATE ITEMS
	**;
	end;
	if r3date_sum=. then do;
	if cg3speaktosp=2 then r3date_sum=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg3speaktosp=1 and max(of cg3date{*})=-1 then r3date_sum=-3; ** PROXY SAYS CAN SPEAK TO
	SP BUT SP UNABLE TO ANSWER **;
	end;
	r3date_sumr=r3date_sum;
	if r3date_sum=-2 then r3date_sumr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if r3date_sum=-3 then r3date_sumr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 6) PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT **;
	array cg3pres {*} cg3presidna1 cg3presidna3 cg3vpname1 cg3vpname3;
	array r3pres_item {*} r3preslast r3presfirst r3vplast r3vpfirst;
	do i=1 to dim(r3pres_item);
	if cg3pres{i} > 0 then r3pres_item{i}=cg3pres{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES
	-1,-9 LEFT MISSING **;
	if cg3pres{i} in (-7,2) then r3pres_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r3presvp=sum(of r3pres_item{*}); ** COUNT CORRECT PRESIDENT/VEEP NAME ITEMS
	**;
	end;
	if r3presvp=. then do;
	if cg3speaktosp=2 then r3presvp=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg3speaktosp=1 and max(of cg3pres{*})=-1 then r3presvp=-3; ** PROXY SAYS CAN SPEAK TO SP
	BUT SP UNABLE TO ANSWER **;
	end;
	r3presvpr=r3presvp;
	if r3presvp=-2 then r3presvpr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if r3presvp=-3 then r3presvpr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 7) ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING **;
	r3date_prvp=sum(r3date_sumr,r3presvpr);
	** 8) EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE **;
	r3clock_scorer=cg3dclkdraw;
	if cg3dclkdraw in (-2,-9) then r3clock_scorer=.;
	if cg3dclkdraw in (-3,-4,-7) then r3clock_scorer=0;
	** IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK **;
	if cg3dclkdraw=-9 and cg3speaktosp=1 then r3clock_scorer=2; ** IF PROXY SAID CAN ASK SP **;
	if cg3dclkdraw=-9 and cg3speaktosp=-1 then r3clock_scorer=3; ** IF SELF RESPONDENT **;
	** 9) MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL **;
	array cg3recall {*} cg3dwrdimmrc cg3dwrddlyrc;
	array r3word_recall {*} r3irecall r3drecall;
	do i=1 to dim(r3word_recall);
	r3word_recall{i}=cg3recall{i};
	if cg3recall{i} in (-2,-1) then r3word_recall{i}=.;
	if cg3recall{i} in (-7,-3) then r3word_recall{i}=0;
	r3wordrecall0_20=sum(of r3word_recall{*});
	end;
	** 10) CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE **;
	** I.E. PROXY BUT PROXY SAYS CAN ASK SP, NOT FQ ONLY, NOT NH **;
	if 1 < r3clock_scorer <=5 then r3clock65=0;
	if 0 <= r3clock_scorer <=1 then r3clock65=1;
	if 3 < r3wordrecall0_20 <=20 then r3word65=0;
	if 0 <= r3wordrecall0_20 <=3 then r3word65=1;
	if 3 < r3date_prvp <= 8 then r3datena65=0;
	if 0 <= r3date_prvp <= 3 then r3datena65=1;
	** 10) CREATE COGNITIVE DOMAIN SCORE **;
	array r3domains {*} r3clock65 r3word65 r3datena65;
	do i=1 to dim(r3domains);
	r3domain65=sum(of r3domains{*});
	end;
	** 11) UPDATE COGNITIVE CLASSIFICATION **;
	if r3demclas=. and cg3speaktosp in (-1,1) then do;
	if 2 <= r3domain65 <=3 then r3demclas=1; ** PROBABLE DEMENTIA **;
	if r3domain65 =1 then r3demclas=2; ** POSSIBLE DEMENTIA **;
	if r3domain65 =0 then r3demclas=3; ** NO DEMENTIA **;
	end;
	format r3demclas demclas. r3ad8_dem ad8dem. r3clock65 clockf. r3word65 wordrecf. r3datena65 dateprf.;
	Run;


	/** DEMENTIA CLASSIFICATION VARIABLE - ROUND 4 **
	** NOTE: The input file to run this code is the NHATS_Round_2_SP_File**
	** DATE STEP CODE FOR CREATING DEMENTIA CLASSIFICATION VARIABLE **/
	Data demclas_R4; Set NHATS_R4;
	length r4demclas
	r4ad8_dem r4ad8_1-r4ad8_8 r4ad8miss_1-r4ad8miss_8 r4ad8_score r4ad8_miss
	r4date_mon r4date_day r4date_yr r4date_dow r4date_sum r4date_sumr
	r4preslast r4presfirst r4vplast r4vpfirst r4presvp r4presvpr r4date_prvp
	r4clock_scorer r4irecall r4drecall r4wordrecall0_20
	r4clock65 r4word65 r4datena65 r4domain65
	3;
	label r4demclas="R4 NHATS Dementia Classification 65+";
		/*if cg2dwrdimmrc = 10 and cg2dwrddlyrc = -3 then cg2dwrdimmrc = -3; **USE THIS LINE TO FIX A CODING ERROR IN ROUND 2 ONLY***/
	** 1) SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED) **;
	if r4dresid=7 then r4demclas=-9;
	if r4dresid in (6,8) then r4demclas=-1;
	** 2) CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY **;
	if hc4disescn9 in (1,7) and is4resptype in (1,2) then r4demclas=1;
	** 3a) CODE AD8_SCORE **;
	array r4think {*} cp4chgthink1-cp4chgthink8; ** QUESTIONNAIRE ITEMS **;
	array r4ad8item {*} r4ad8_1-r4ad8_8;
	array r4ad8miss {*} r4ad8miss_1-r4ad8miss_8;
	r4ad8_score =-1;
	r4ad8_miss =-1;
	do i=1 to dim(r4ad8item);
	** INITIALIZE COUNTS TO NOT APPLICABLE**;
	r4ad8item{i}=-1;
	r4ad8miss{i}=-1;
	** ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY
	REPORTED DIAGNOSIS **;
	if is4resptype=2 and r4demclas=. then do;
	r4ad8item{i}=.;
	if r4think{i} in (1,3) then r4ad8item{i}=1; ** PROXY REPORTS A CHANGE OR ALZ/DEMENTIA **;
	else if r4think{i}=2 then r4ad8item{i}=0; ** PROXY REPORTS NO CHANGE **;
	r4ad8_score=sum(of r4ad8item{*}); ** COUNT AD8 ITEMS **;
	if r4ad8item{i} in (0,1) then r4ad8miss{i}=0;
	else if r4ad8item{i}=. then r4ad8miss{i}=1;
	r4ad8_miss=sum(of r4ad8miss{*}); ** COUNT MISSING AD8 ITEMS **;
	end;
	end;
	** 3b) CODE AD8 DEMENTIA CLASS **;
	if cp4dad8dem=1 and is4resptype=2 and r4demclas=. then r4ad8_score=8;
	if r4ad8_score>=2 then r4ad8_dem=1 ; ** IF SCORE >=2 THEN MEETS AD8 CRITERION **;
	if r4ad8_score in (0,1) or (r4ad8_miss=8 and r4ad8_dem=.) then r4ad8_dem=2; ** IF SCORE IS 0 OR 1 OR ALL ITEMS MISSING
	THEN DOES NOT MEET AD8 CRITERION **;
	** 4) UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS **;
	if r4demclas=. then do;
	if r4ad8_dem=1 then r4demclas=1; ** PROBABLE BASED ON AD8 SCORE **;
	if r4ad8_dem=2 and cg4speaktosp=2 then r4demclas=3; ** NO DIAGNOSIS, DOES NOT MEET AD8
	CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS **;
	end;
	** 5) CODE DATE ITEMS AND COUNT **;
		/*array cg3date {*} cg3todaydat1-cg3todaydat4; **USE THIS LINE FOR ROUNDS 1-3, 5**/
	array cg4date {*} cg4todaydat1 cg4todaydat2 cg4todaydat3 cg4todaydat5; **USE THIS LINE FOR ROUND 4**;
	array r4date_item {*} r4date_mon r4date_day r4date_yr r4date_dow;
	do i=1 to dim(r4date_item);
	if cg4date{i} > 0 then r4date_item{i}=cg4date{i}; *** CODE ONLY YES/NO RESPONSES: MISSING/N.A.
	CODES -1,-9 LEFT MISSING **;
	if cg4date{i} in (-7,2) then r4date_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r4date_sum=sum(of r4date_item{*}); ** COUNT CORRECT DATE ITEMS
	**;
	end;
	if r4date_sum=. then do;
	if cg4speaktosp=2 then r4date_sum=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg4speaktosp=1 and max(of cg4date{*})=-1 then r4date_sum=-3; ** PROXY SAYS CAN SPEAK TO
	SP BUT SP UNABLE TO ANSWER **;
	end;
	r4date_sumr=r4date_sum;
	if r4date_sum=-2 then r4date_sumr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if r4date_sum=-3 then r4date_sumr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 6) PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT **;
	array cg4pres {*} cg4presidna1 cg4presidna3 cg4vpname1 cg4vpname3;
	array r4pres_item {*} r4preslast r4presfirst r4vplast r4vpfirst;
	do i=1 to dim(r4pres_item);
	if cg4pres{i} > 0 then r4pres_item{i}=cg4pres{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES
	-1,-9 LEFT MISSING **;
	if cg4pres{i} in (-7,2) then r4pres_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r4presvp=sum(of r4pres_item{*}); ** COUNT CORRECT PRESIDENT/VEEP NAME ITEMS
	**;
	end;
	if r4presvp=. then do;
	if cg4speaktosp=2 then r4presvp=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg4speaktosp=1 and max(of cg4pres{*})=-1 then r4presvp=-3; ** PROXY SAYS CAN SPEAK TO SP
	BUT SP UNABLE TO ANSWER **;
	end;
	r4presvpr=r4presvp;
	if r4presvp=-2 then r4presvpr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if r4presvp=-3 then r4presvpr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 7) ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING **;
	r4date_prvp=sum(r4date_sumr,r4presvpr);
	** 8) EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE **;
	r4clock_scorer=cg4dclkdraw;
	if cg4dclkdraw in (-2,-9) then r4clock_scorer=.;
	if cg4dclkdraw in (-3,-4,-7) then r4clock_scorer=0;
	** IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK **;
	if cg4dclkdraw=-9 and cg4speaktosp=1 then r4clock_scorer=2; ** IF PROXY SAID CAN ASK SP **;
	if cg4dclkdraw=-9 and cg4speaktosp=-1 then r4clock_scorer=3; ** IF SELF RESPONDENT **;
	** 9) MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL **;
	array cg4recall {*} cg4dwrdimmrc cg4dwrddlyrc;
	array r4word_recall {*} r4irecall r4drecall;
	do i=1 to dim(r4word_recall);
	r4word_recall{i}=cg4recall{i};
	if cg4recall{i} in (-2,-1) then r4word_recall{i}=.;
	if cg4recall{i} in (-7,-3) then r4word_recall{i}=0;
	r4wordrecall0_20=sum(of r4word_recall{*});
	end;
	** 10) CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE **;
	** I.E. PROXY BUT PROXY SAYS CAN ASK SP, NOT FQ ONLY, NOT NH **;
	if 1 < r4clock_scorer <=5 then r4clock65=0;
	if 0 <= r4clock_scorer <=1 then r4clock65=1;
	if 3 < r4wordrecall0_20 <=20 then r4word65=0;
	if 0 <= r4wordrecall0_20 <=3 then r4word65=1;
	if 3 < r4date_prvp <= 8 then r4datena65=0;
	if 0 <= r4date_prvp <= 3 then r4datena65=1;
	** 10) CREATE COGNITIVE DOMAIN SCORE **;
	array r4domains {*} r4clock65 r4word65 r4datena65;
	do i=1 to dim(r4domains);
	r4domain65=sum(of r4domains{*});
	end;
	** 11) UPDATE COGNITIVE CLASSIFICATION **;
	if r4demclas=. and cg4speaktosp in (-1,1) then do;
	if 2 <= r4domain65 <=3 then r4demclas=1; ** PROBABLE DEMENTIA **;
	if r4domain65 =1 then r4demclas=2; ** POSSIBLE DEMENTIA **;
	if r4domain65 =0 then r4demclas=3; ** NO DEMENTIA **;
	end;
	format r4demclas demclas. r4ad8_dem ad8dem. r4clock65 clockf. r4word65 wordrecf. r4datena65 dateprf.;
	Run;


	/** DEMENTIA CLASSIFICATION VARIABLE - ROUND 5 **
	** NOTE: The input file to run this code is the NHATS_Round_2_SP_File**
	** DATE STEP CODE FOR CREATING DEMENTIA CLASSIFICATION VARIABLE **/
	Data demclas_R5; Set NHATS_R5;
	length r5demclas
	r5ad8_dem r5ad8_1-r5ad8_8 r5ad8miss_1-r5ad8miss_8 r5ad8_score r5ad8_miss
	r5date_mon r5date_day r5date_yr r5date_dow r5date_sum r5date_sumr
	r5preslast r5presfirst r5vplast r5vpfirst r5presvp r5presvpr r5date_prvp
	r5clock_scorer r5irecall r5drecall r5wordrecall0_20
	r5clock65 r5word65 r5datena65 r5domain65
	3;
	label r5demclas="R5 NHATS Dementia Classification 65+";
		/*if cg2dwrdimmrc = 10 and cg2dwrddlyrc = -3 then cg2dwrdimmrc = -3; **USE THIS LINE TO FIX A CODING ERROR IN ROUND 2 ONLY**/
	** 1) SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED) **;
	if r5dresid=7 then r5demclas=-9;
	if r5dresid in (6,8) then r5demclas=-1;
	** 2) CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY **;
	if hc5disescn9 in (1,7) and is5resptype in (1,2) then r5demclas=1;
	** 3a) CODE AD8_SCORE **;
	array r5think {*} cp5chgthink1-cp5chgthink8; ** QUESTIONNAIRE ITEMS **;
	array r5ad8item {*} r5ad8_1-r5ad8_8;
	array r5ad8miss {*} r5ad8miss_1-r5ad8miss_8;
	r5ad8_score =-1;
	r5ad8_miss =-1;
	do i=1 to dim(r5ad8item);
	** INITIALIZE COUNTS TO NOT APPLICABLE**;
	r5ad8item{i}=-1;
	r5ad8miss{i}=-1;
	** ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY
	REPORTED DIAGNOSIS **;
	if is5resptype=2 and r5demclas=. then do;
	r5ad8item{i}=.;
	if r5think{i} in (1,3) then r5ad8item{i}=1; ** PROXY REPORTS A CHANGE OR ALZ/DEMENTIA **;
	else if r5think{i}=2 then r5ad8item{i}=0; ** PROXY REPORTS NO CHANGE **;
	r5ad8_score=sum(of r5ad8item{*}); ** COUNT AD8 ITEMS **;
	if r5ad8item{i} in (0,1) then r5ad8miss{i}=0;
	else if r5ad8item{i}=. then r5ad8miss{i}=1;
	r5ad8_miss=sum(of r5ad8miss{*}); ** COUNT MISSING AD8 ITEMS **;
	end;
	end;
	** 3b) CODE AD8 DEMENTIA CLASS **;
	if cp5dad8dem=1 and is5resptype=2 and r5demclas=. then r5ad8_score=8;
	if r5ad8_score>=2 then r5ad8_dem=1 ; ** IF SCORE >=2 THEN MEETS AD8 CRITERION **;
	if r5ad8_score in (0,1) or (r5ad8_miss=8 and r5ad8_dem=.) then r5ad8_dem=2; ** IF SCORE IS 0 OR 1 OR ALL ITEMS MISSING
	THEN DOES NOT MEET AD8 CRITERION **;
	** 4) UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS **;
	if r5demclas=. then do;
	if r5ad8_dem=1 then r5demclas=1; ** PROBABLE BASED ON AD8 SCORE **;
	if r5ad8_dem=2 and cg5speaktosp=2 then r5demclas=3; ** NO DIAGNOSIS, DOES NOT MEET AD8
	CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS **;
	end;
	** 5) CODE DATE ITEMS AND COUNT **;
	array cg5date {*} cg5todaydat1-cg5todaydat4; **USE THIS LINE FOR ROUNDS 1-3, 5**;
		/*array cg4date {*} cg4todaydat1 cg4todaydat2 cg4todaydat3 cg4todaydat5; **USE THIS LINE FOR ROUND 4**/
	array r5date_item {*} r5date_mon r5date_day r5date_yr r5date_dow;
	do i=1 to dim(r5date_item);
	if cg5date{i} > 0 then r5date_item{i}=cg5date{i}; *** CODE ONLY YES/NO RESPONSES: MISSING/N.A.
	CODES -1,-9 LEFT MISSING **;
	if cg5date{i} in (-7,2) then r5date_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r5date_sum=sum(of r5date_item{*}); ** COUNT CORRECT DATE ITEMS
	**;
	end;
	if r5date_sum=. then do;
	if cg5speaktosp=2 then r5date_sum=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg5speaktosp=1 and max(of cg5date{*})=-1 then r5date_sum=-3; ** PROXY SAYS CAN SPEAK TO
	SP BUT SP UNABLE TO ANSWER **;
	end;
	r5date_sumr=r5date_sum;
	if r5date_sum=-2 then r5date_sumr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if r5date_sum=-3 then r5date_sumr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 6) PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT **;
	array cg5pres {*} cg5presidna1 cg5presidna3 cg5vpname1 cg5vpname3;
	array r5pres_item {*} r5preslast r5presfirst r5vplast r5vpfirst;
	do i=1 to dim(r5pres_item);
	if cg5pres{i} > 0 then r5pres_item{i}=cg5pres{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES
	-1,-9 LEFT MISSING **;
	if cg5pres{i} in (-7,2) then r5pres_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r5presvp=sum(of r5pres_item{*}); ** COUNT CORRECT PRESIDENT/VEEP NAME ITEMS
	**;
	end;
	if r5presvp=. then do;
	if cg5speaktosp=2 then r5presvp=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg5speaktosp=1 and max(of cg5pres{*})=-1 then r5presvp=-3; ** PROXY SAYS CAN SPEAK TO SP
	BUT SP UNABLE TO ANSWER **;
	end;
	r5presvpr=r5presvp;
	if r5presvp=-2 then r5presvpr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if r5presvp=-3 then r5presvpr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 7) ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING **;
	r5date_prvp=sum(r5date_sumr,r5presvpr);
	** 8) EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE **;
	r5clock_scorer=cg5dclkdraw;
	if cg5dclkdraw in (-2,-9) then r5clock_scorer=.;
	if cg5dclkdraw in (-3,-4,-7) then r5clock_scorer=0;
	** IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK **;
	if cg5dclkdraw=-9 and cg5speaktosp=1 then r5clock_scorer=2; ** IF PROXY SAID CAN ASK SP **;
	if cg5dclkdraw=-9 and cg5speaktosp=-1 then r5clock_scorer=3; ** IF SELF RESPONDENT **;
	** 9) MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL **;
	array cg5recall {*} cg5dwrdimmrc cg5dwrddlyrc;
	array r5word_recall {*} r5irecall r5drecall;
	do i=1 to dim(r5word_recall);
	r5word_recall{i}=cg5recall{i};
	if cg5recall{i} in (-2,-1) then r5word_recall{i}=.;
	if cg5recall{i} in (-7,-3) then r5word_recall{i}=0;
	r5wordrecall0_20=sum(of r5word_recall{*});
	end;
	** 10) CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE **;
	** I.E. PROXY BUT PROXY SAYS CAN ASK SP, NOT FQ ONLY, NOT NH **;
	if 1 < r5clock_scorer <=5 then r5clock65=0;
	if 0 <= r5clock_scorer <=1 then r5clock65=1;
	if 3 < r5wordrecall0_20 <=20 then r5word65=0;
	if 0 <= r5wordrecall0_20 <=3 then r5word65=1;
	if 3 < r5date_prvp <= 8 then r5datena65=0;
	if 0 <= r5date_prvp <= 3 then r5datena65=1;
	** 10) CREATE COGNITIVE DOMAIN SCORE **;
	array r5domains {*} r5clock65 r5word65 r5datena65;
	do i=1 to dim(r5domains);
	r5domain65=sum(of r5domains{*});
	end;
	** 11) UPDATE COGNITIVE CLASSIFICATION **;
	if r5demclas=. and cg5speaktosp in (-1,1) then do;
	if 2 <= r5domain65 <=3 then r5demclas=1; ** PROBABLE DEMENTIA **;
	if r5domain65 =1 then r5demclas=2; ** POSSIBLE DEMENTIA **;
	if r5domain65 =0 then r5demclas=3; ** NO DEMENTIA **;
	format r5demclas demclas. r5ad8_dem ad8dem. r5clock65 clockf. r5word65 wordrecf. r5datena65 dateprf.;
	end;
	Run;


	/** DEMENTIA CLASSIFICATION VARIABLE - ROUND 6 **
	** NOTE: The input file to run this code is the NHATS_Round_2_SP_File**
	** DATE STEP CODE FOR CREATING DEMENTIA CLASSIFICATION VARIABLE **/
	Data demclas_R6; Set NHATS_R6;
	length r6demclas
	r6ad8_dem r6ad8_1-r6ad8_8 r6ad8miss_1-r6ad8miss_8 r6ad8_score r6ad8_miss
	r6date_mon r6date_day r6date_yr r6date_dow r6date_sum r6date_sumr
	r6preslast r6presfirst r6vplast r6vpfirst r6presvp r6presvpr r6date_prvp
	r6clock_scorer r6irecall r6drecall r6wordrecall0_20
	r6clock65 r6word65 r6datena65 r6domain65
	3;
	label r6demclas="R6 NHATS Dementia Classification 65+";
		/*if cg2dwrdimmrc = 10 and cg2dwrddlyrc = -3 then cg2dwrdimmrc = -3; **USE THIS LINE TO FIX A CODING ERROR IN ROUND 2 ONLY**/
	** 1) SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED) **;
	if r6dresid=7 then r6demclas=-9;
	if r6dresid in (6,8) then r6demclas=-1;
	** 2) CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY **;
	if hc6disescn9 in (1,7) and is6resptype in (1,2) then r6demclas=1;
	** 3a) CODE AD8_SCORE **;
	array r6think {*} cp6chgthink1-cp6chgthink8; ** QUESTIONNAIRE ITEMS **;
	array r6ad8item {*} r6ad8_1-r6ad8_8;
	array r6ad8miss {*} r6ad8miss_1-r6ad8miss_8;
	r6ad8_score =-1;
	r6ad8_miss =-1;
	do i=1 to dim(r6ad8item);
	** INITIALIZE COUNTS TO NOT APPLICABLE**;
	r6ad8item{i}=-1;
	r6ad8miss{i}=-1;
	** ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY
	REPORTED DIAGNOSIS **;
	if is6resptype=2 and r6demclas=. then do;
	r6ad8item{i}=.;
	if r6think{i} in (1,3) then r6ad8item{i}=1; ** PROXY REPORTS A CHANGE OR ALZ/DEMENTIA **;
	else if r6think{i}=2 then r6ad8item{i}=0; ** PROXY REPORTS NO CHANGE **;
	r6ad8_score=sum(of r6ad8item{*}); ** COUNT AD8 ITEMS **;
	if r6ad8item{i} in (0,1) then r6ad8miss{i}=0;
	else if r6ad8item{i}=. then r6ad8miss{i}=1;
	r6ad8_miss=sum(of r6ad8miss{*}); ** COUNT MISSING AD8 ITEMS **;
	end;
	end;
	** 3b) CODE AD8 DEMENTIA CLASS **;
	if cp6dad8dem=1 and is6resptype=2 and r6demclas=. then r6ad8_score=8;
	if r6ad8_score>=2 then r6ad8_dem=1 ; ** IF SCORE >=2 THEN MEETS AD8 CRITERION **;
	if r6ad8_score in (0,1) or (r6ad8_miss=8 and r6ad8_dem=.) then r6ad8_dem=2; ** IF SCORE IS 0 OR 1 OR ALL ITEMS MISSING
	THEN DOES NOT MEET AD8 CRITERION **;
	** 4) UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS **;
	if r6demclas=. then do;
	if r6ad8_dem=1 then r6demclas=1; ** PROBABLE BASED ON AD8 SCORE **;
	if r6ad8_dem=2 and cg6speaktosp=2 then r6demclas=3; ** NO DIAGNOSIS, DOES NOT MEET AD8
	CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS **;
	end;
	** 5) CODE DATE ITEMS AND COUNT **;
	array cg6date {*} cg6todaydat1-cg6todaydat4; **USE THIS LINE FOR ROUNDS 1-3, 5 -- TMM verified R6 vars correct 1/14/2020 **;
		/*array cg4date {*} cg4todaydat1 cg4todaydat2 cg4todaydat3 cg4todaydat5; **USE THIS LINE FOR ROUND 4**/
	array r6date_item {*} r6date_mon r6date_day r6date_yr r6date_dow;
	do i=1 to dim(r6date_item);
	if cg6date{i} > 0 then r6date_item{i}=cg6date{i}; *** CODE ONLY YES/NO RESPONSES: MISSING/N.A.
	CODES -1,-9 LEFT MISSING **;
	if cg6date{i} in (-7,2) then r6date_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r6date_sum=sum(of r6date_item{*}); ** COUNT CORRECT DATE ITEMS
	**;
	end;
	if r6date_sum=. then do;
	if cg6speaktosp=2 then r6date_sum=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg6speaktosp=1 and max(of cg6date{*})=-1 then r6date_sum=-3; ** PROXY SAYS CAN SPEAK TO
	SP BUT SP UNABLE TO ANSWER **;
	end;
	r6date_sumr=r6date_sum;
	if r6date_sum=-2 then r6date_sumr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if r6date_sum=-3 then r6date_sumr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 6) PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT **;
	array cg6pres {*} cg6presidna1 cg6presidna3 cg6vpname1 cg6vpname3;
	array r6pres_item {*} r6preslast r6presfirst r6vplast r6vpfirst;
	do i=1 to dim(r6pres_item);
	if cg6pres{i} > 0 then r6pres_item{i}=cg6pres{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES
	-1,-9 LEFT MISSING **;
	if cg6pres{i} in (-7,2) then r6pres_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r6presvp=sum(of r6pres_item{*}); ** COUNT CORRECT PRESIDENT/VEEP NAME ITEMS
	**;
	end;
	if r6presvp=. then do;
	if cg6speaktosp=2 then r6presvp=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg6speaktosp=1 and max(of cg6pres{*})=-1 then r5presvp=-3; ** PROXY SAYS CAN SPEAK TO SP
	BUT SP UNABLE TO ANSWER **;
	end;
	r6presvpr=r6presvp;
	if r6presvp=-2 then r6presvpr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if r6presvp=-3 then r6presvpr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 7) ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING **;
	r6date_prvp=sum(r6date_sumr,r6presvpr);
	** 8) EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE **;
	r6clock_scorer=cg6dclkdraw;
	if cg6dclkdraw in (-2,-9) then r6clock_scorer=.;
	if cg6dclkdraw in (-3,-4,-7) then r6clock_scorer=0;
	** IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK **;
	if cg6dclkdraw=-9 and cg6speaktosp=1 then r6clock_scorer=2; ** IF PROXY SAID CAN ASK SP **;
	if cg6dclkdraw=-9 and cg6speaktosp=-1 then r6clock_scorer=3; ** IF SELF RESPONDENT **;
	** 9) MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL **;
	array cg6recall {*} cg6dwrdimmrc cg6dwrddlyrc;
	array r6word_recall {*} r6irecall r6drecall;
	do i=1 to dim(r6word_recall);
	r6word_recall{i}=cg6recall{i};
	if cg6recall{i} in (-2,-1) then r6word_recall{i}=.;
	if cg6recall{i} in (-7,-3) then r6word_recall{i}=0;
	r6wordrecall0_20=sum(of r6word_recall{*});
	end;
	** 10) CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE **;
	** I.E. PROXY BUT PROXY SAYS CAN ASK SP, NOT FQ ONLY, NOT NH **;
	if 1 < r6clock_scorer <=5 then r6clock65=0;
	if 0 <= r6clock_scorer <=1 then r6clock65=1;
	if 3 < r6wordrecall0_20 <=20 then r6word65=0;
	if 0 <= r6wordrecall0_20 <=3 then r6word65=1;
	if 3 < r6date_prvp <= 8 then r6datena65=0;
	if 0 <= r6date_prvp <= 3 then r6datena65=1;
	** 10) CREATE COGNITIVE DOMAIN SCORE **;
	array r6domains {*} r6clock65 r6word65 r6datena65;
	do i=1 to dim(r6domains);
	r6domain65=sum(of r6domains{*});
	end;
	** 11) UPDATE COGNITIVE CLASSIFICATION **;
	if r6demclas=. and cg6speaktosp in (-1,1) then do;
	if 2 <= r6domain65 <=3 then r6demclas=1; ** PROBABLE DEMENTIA **;
	if r6domain65 =1 then r6demclas=2; ** POSSIBLE DEMENTIA **;
	if r6domain65 =0 then r6demclas=3; ** NO DEMENTIA **;
	format r6demclas demclas. r6ad8_dem ad8dem. r6clock65 clockf. r6word65 wordrecf. r6datena65 dateprf.;
	end;
	Run;


	/** DEMENTIA CLASSIFICATION VARIABLE - ROUND 7 **
	** NOTE: The input file to run this code is the NHATS_Round_2_SP_File**
	** DATE STEP CODE FOR CREATING DEMENTIA CLASSIFICATION VARIABLE **/
	Data demclas_R7; Set NHATS_R7;
	length r7demclas
	r7ad8_dem r7ad8_1-r7ad8_8 r7ad8miss_1-r7ad8miss_8 r7ad8_score r7ad8_miss
	r7date_mon r7date_day r7date_yr r7date_dow r7date_sum r7date_sumr
	r7preslast r7presfirst r7vplast r7vpfirst r7presvp r7presvpr r7date_prvp
	r7clock_scorer r7irecall r7drecall r7wordrecall0_20
	r7clock65 r7word65 r7datena65 r7domain65
	3;
	label r7demclas="R7 NHATS Dementia Classification 65+";
		/*if cg2dwrdimmrc = 10 and cg2dwrddlyrc = -3 then cg2dwrdimmrc = -3; **USE THIS LINE TO FIX A CODING ERROR IN ROUND 2 ONLY**/
	** 1) SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED) **;
	if r7dresid=7 then r7demclas=-9;
	if r7dresid in (6,8) then r7demclas=-1;
	** 2) CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY **;
	if hc7disescn9 in (1,7) and is7resptype in (1,2) then r7demclas=1;
	** 3a) CODE AD8_SCORE **;
	array r7think {*} cp7chgthink1-cp7chgthink8; ** QUESTIONNAIRE ITEMS **;
	array r7ad8item {*} r7ad8_1-r7ad8_8;
	array r7ad8miss {*} r7ad8miss_1-r7ad8miss_8;
	r7ad8_score =-1;
	r7ad8_miss =-1;
	do i=1 to dim(r7ad8item);
	** INITIALIZE COUNTS TO NOT APPLICABLE**;
	r7ad8item{i}=-1;
	r7ad8miss{i}=-1;
	** ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY
	REPORTED DIAGNOSIS **;
	if is7resptype=2 and r7demclas=. then do;
	r7ad8item{i}=.;
	if r7think{i} in (1,3) then r7ad8item{i}=1; ** PROXY REPORTS A CHANGE OR ALZ/DEMENTIA **;
	else if r7think{i}=2 then r7ad8item{i}=0; ** PROXY REPORTS NO CHANGE **;
	r7ad8_score=sum(of r7ad8item{*}); ** COUNT AD8 ITEMS **;
	if r7ad8item{i} in (0,1) then r7ad8miss{i}=0;
	else if r7ad8item{i}=. then r7ad8miss{i}=1;
	r7ad8_miss=sum(of r7ad8miss{*}); ** COUNT MISSING AD8 ITEMS **;
	end;
	end;
	** 3b) CODE AD8 DEMENTIA CLASS **;
	if cp7dad8dem=1 and is7resptype=2 and r7demclas=. then r7ad8_score=8;
	if r7ad8_score>=2 then r7ad8_dem=1 ; ** IF SCORE >=2 THEN MEETS AD8 CRITERION **;
	if r7ad8_score in (0,1) or (r7ad8_miss=8 and r7ad8_dem=.) then r7ad8_dem=2; ** IF SCORE IS 0 OR 1 OR ALL ITEMS MISSING
	THEN DOES NOT MEET AD8 CRITERION **;
	** 4) UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS **;
	if r7demclas=. then do;
	if r7ad8_dem=1 then r7demclas=1; ** PROBABLE BASED ON AD8 SCORE **;
	if r7ad8_dem=2 and cg7speaktosp=2 then r7demclas=3; ** NO DIAGNOSIS, DOES NOT MEET AD8
	CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS **;
	end;
	** 5) CODE DATE ITEMS AND COUNT **;
	array cg7date {*} cg7todaydat1-cg7todaydat4; **USE THIS LINE FOR ROUNDS 1-3, 5 -- TMM verified correct R7 vars 1/14/2020**;
		/*array cg4date {*} cg4todaydat1 cg4todaydat2 cg4todaydat3 cg4todaydat5; **USE THIS LINE FOR ROUND 4**/
	array r7date_item {*} r7date_mon r7date_day r7date_yr r7date_dow;
	do i=1 to dim(r7date_item);
	if cg7date{i} > 0 then r7date_item{i}=cg7date{i}; *** CODE ONLY YES/NO RESPONSES: MISSING/N.A.
	CODES -1,-9 LEFT MISSING **;
	if cg7date{i} in (-7,2) then r7date_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r7date_sum=sum(of r7date_item{*}); ** COUNT CORRECT DATE ITEMS
	**;
	end;
	if r7date_sum=. then do;
	if cg7speaktosp=2 then r7date_sum=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg7speaktosp=1 and max(of cg7date{*})=-1 then r7date_sum=-3; ** PROXY SAYS CAN SPEAK TO
	SP BUT SP UNABLE TO ANSWER **;
	end;
	r7date_sumr=r7date_sum;
	if r7date_sum=-2 then r7date_sumr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if r7date_sum=-3 then r7date_sumr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 6) PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT **;
	array cg7pres {*} cg7presidna1 cg7presidna3 cg7vpname1 cg7vpname3;
	array r7pres_item {*} r7preslast r7presfirst r7vplast r7vpfirst;
	do i=1 to dim(r7pres_item);
	if cg7pres{i} > 0 then r7pres_item{i}=cg7pres{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES
	-1,-9 LEFT MISSING **;
	if cg7pres{i} in (-7,2) then r7pres_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r7presvp=sum(of r7pres_item{*}); ** COUNT CORRECT PRESIDENT/VEEP NAME ITEMS
	**;
	end;
	if r7presvp=. then do;
	if cg7speaktosp=2 then r7presvp=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg7speaktosp=1 and max(of cg7pres{*})=-1 then r7presvp=-3; ** PROXY SAYS CAN SPEAK TO SP
	BUT SP UNABLE TO ANSWER **;
	end;
	r7presvpr=r7presvp;
	if r7presvp=-2 then r7presvpr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if r7presvp=-3 then r7presvpr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 7) ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING **;
	r7date_prvp=sum(r7date_sumr,r7presvpr);
	** 8) EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE **;
	r7clock_scorer=cg7dclkdraw;
	if cg7dclkdraw in (-2,-9) then r7clock_scorer=.;
	if cg7dclkdraw in (-3,-4,-7) then r7clock_scorer=0;
	** IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK **;
	if cg7dclkdraw=-9 and cg7speaktosp=1 then r7clock_scorer=2; ** IF PROXY SAID CAN ASK SP **;
	if cg7dclkdraw=-9 and cg7speaktosp=-1 then r7clock_scorer=3; ** IF SELF RESPONDENT **;
	** 9) MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL **;
	array cg7recall {*} cg7dwrdimmrc cg7dwrddlyrc;
	array r7word_recall {*} r7irecall r7drecall;
	do i=1 to dim(r7word_recall);
	r7word_recall{i}=cg7recall{i};
	if cg7recall{i} in (-2,-1) then r7word_recall{i}=.;
	if cg7recall{i} in (-7,-3) then r7word_recall{i}=0;
	r7wordrecall0_20=sum(of r7word_recall{*});
	end;
	** 10) CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE **;
	** I.E. PROXY BUT PROXY SAYS CAN ASK SP, NOT FQ ONLY, NOT NH **;
	if 1 < r7clock_scorer <=5 then r7clock65=0;
	if 0 <= r7clock_scorer <=1 then r7clock65=1;
	if 3 < r7wordrecall0_20 <=20 then r7word65=0;
	if 0 <= r7wordrecall0_20 <=3 then r7word65=1;
	if 3 < r7date_prvp <= 8 then r7datena65=0;
	if 0 <= r7date_prvp <= 3 then r7datena65=1;
	** 10) CREATE COGNITIVE DOMAIN SCORE **;
	array r7domains {*} r7clock65 r7word65 r7datena65;
	do i=1 to dim(r7domains);
	r7domain65=sum(of r7domains{*});
	end;
	** 11) UPDATE COGNITIVE CLASSIFICATION **;
	if r7demclas=. and cg7speaktosp in (-1,1) then do;
	if 2 <= r7domain65 <=3 then r7demclas=1; ** PROBABLE DEMENTIA **;
	if r7domain65 =1 then r7demclas=2; ** POSSIBLE DEMENTIA **;
	if r7domain65 =0 then r7demclas=3; ** NO DEMENTIA **;
	format r7demclas demclas. r7ad8_dem ad8dem. r7clock65 clockf. r7word65 wordrecf. r7datena65 dateprf.;
	end;
	Run;


	/** DEMENTIA CLASSIFICATION VARIABLE - ROUND 8 **
	** NOTE: The input file to run this code is the NHATS_Round_2_SP_File**
	** DATE STEP CODE FOR CREATING DEMENTIA CLASSIFICATION VARIABLE **/
	Data demclas_R8; Set NHATS_R8;
	length r8demclas
	r8ad8_dem r8ad8_1-r8ad8_8 r8ad8miss_1-r8ad8miss_8 r8ad8_score r8ad8_miss
	r8date_mon r8date_day r8date_yr r8date_dow r8date_sum r8date_sumr
	r8preslast r8presfirst r8vplast r8vpfirst r8presvp r8presvpr r8date_prvp
	r8clock_scorer r8irecall r8drecall r8wordrecall0_20
	r8clock65 r8word65 r8datena65 r8domain65
	3;
	label r8demclas="R8 NHATS Dementia Classification 65+";
		/*if cg2dwrdimmrc = 10 and cg2dwrddlyrc = -3 then cg2dwrdimmrc = -3; **USE THIS LINE TO FIX A CODING ERROR IN ROUND 2 ONLY**/
	** 1) SET MISSING (RESIDENTIAL CARE FQ ONLY) AND N.A. (NURSING HOME RESIDENTS, DECEASED) **;
	if r8dresid=7 then r8demclas=-9;
	if r8dresid in (6,8) then r8demclas=-1;
	** 2) CODE PROBABLE IF DEMENTIA DIAGNOSIS REPORTED BY SELF OR PROXY **;
	if hc8disescn9 in (1,7) and is8resptype in (1,2) then r8demclas=1;
	** 3a) CODE AD8_SCORE **;
	array r8think {*} cp8chgthink1-cp8chgthink8; ** QUESTIONNAIRE ITEMS **;
	array r8ad8item {*} r8ad8_1-r8ad8_8;
	array r8ad8miss {*} r8ad8miss_1-r8ad8miss_8;
	r8ad8_score =-1;
	r8ad8_miss =-1;
	do i=1 to dim(r8ad8item);
	** INITIALIZE COUNTS TO NOT APPLICABLE**;
	r8ad8item{i}=-1;
	r8ad8miss{i}=-1;
	** ASSIGN VALUES TO AD8 ITEMS IF PROXY AND DEMENTIA CLASS NOT ALREADY ASSIGNED BY
	REPORTED DIAGNOSIS **;
	if is8resptype=2 and r8demclas=. then do;
	r8ad8item{i}=.;
	if r8think{i} in (1,3) then r8ad8item{i}=1; ** PROXY REPORTS A CHANGE OR ALZ/DEMENTIA **;
	else if r8think{i}=2 then r8ad8item{i}=0; ** PROXY REPORTS NO CHANGE **;
	r8ad8_score=sum(of r8ad8item{*}); ** COUNT AD8 ITEMS **;
	if r8ad8item{i} in (0,1) then r8ad8miss{i}=0;
	else if r8ad8item{i}=. then r8ad8miss{i}=1;
	r8ad8_miss=sum(of r8ad8miss{*}); ** COUNT MISSING AD8 ITEMS **;
	end;
	end;
	** 3b) CODE AD8 DEMENTIA CLASS **;
	if cp8dad8dem=1 and is8resptype=2 and r8demclas=. then r8ad8_score=8;
	if r8ad8_score>=2 then r8ad8_dem=1 ; ** IF SCORE >=2 THEN MEETS AD8 CRITERION **;
	if r8ad8_score in (0,1) or (r8ad8_miss=8 and r8ad8_dem=.) then r8ad8_dem=2; ** IF SCORE IS 0 OR 1 OR ALL ITEMS MISSING
	THEN DOES NOT MEET AD8 CRITERION **;
	** 4) UPDATE DEMENTIA CLASSIFICATION VARIABLE WITH AD8 CLASS **;
	if r8demclas=. then do;
	if r8ad8_dem=1 then r8demclas=1; ** PROBABLE BASED ON AD8 SCORE **;
	if r8ad8_dem=2 and cg8speaktosp=2 then r8demclas=3; ** NO DIAGNOSIS, DOES NOT MEET AD8
	CRITERION, AND PROXY SAYS CANNOT ASK SP COGNITIVE ITEMS **;
	end;
	** 5) CODE DATE ITEMS AND COUNT **;
	array cg8date {*} cg8todaydat1-cg8todaydat4; **USE THIS LINE FOR ROUNDS 1-3, 5 -- TMM verified correct R8 vars 1/14/2020**;
		/*array cg4date {*} cg4todaydat1 cg4todaydat2 cg4todaydat3 cg4todaydat5; **USE THIS LINE FOR ROUND 4**/
	array r8date_item {*} r8date_mon r8date_day r8date_yr r8date_dow;
	do i=1 to dim(r8date_item);
	if cg8date{i} > 0 then r8date_item{i}=cg8date{i}; *** CODE ONLY YES/NO RESPONSES: MISSING/N.A.
	CODES -1,-9 LEFT MISSING **;
	if cg8date{i} in (-7,2) then r8date_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r8date_sum=sum(of r8date_item{*}); ** COUNT CORRECT DATE ITEMS
	**;
	end;
	if r8date_sum=. then do;
	if cg8speaktosp=2 then r8date_sum=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg8speaktosp=1 and max(of cg8date{*})=-1 then r8date_sum=-3; ** PROXY SAYS CAN SPEAK TO
	SP BUT SP UNABLE TO ANSWER **;
	end;
	r8date_sumr=r8date_sum;
	if r8date_sum=-2 then r8date_sumr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if r8date_sum=-3 then r8date_sumr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 6) PRESIDENT AND VICE PRESIDENT NAME ITEMS AND COUNT **;
	array cg8pres {*} cg8presidna1 cg8presidna3 cg8vpname1 cg8vpname3;
	array r8pres_item {*} r8preslast r8presfirst r8vplast r8vpfirst;
	do i=1 to dim(r8pres_item);
	if cg8pres{i} > 0 then r8pres_item{i}=cg8pres{i}; ** CODE ONLY YES/NO RESPONSES: MISSING/N.A. CODES
	-1,-9 LEFT MISSING **;
	if cg8pres{i} in (-7,2) then r8pres_item{i}=0; ** 2:NO/DK OR -7:REFUSED RECODED TO 0:NO/DK/RF
	**;
	r8presvp=sum(of r8pres_item{*}); ** COUNT CORRECT PRESIDENT/VEEP NAME ITEMS
	**;
	end;
	if r8presvp=. then do;
	if cg8speaktosp=2 then r8presvp=-2; ** PROXY SAYS CAN'T SPEAK TO SP
	**;
	else if cg8speaktosp=1 and max(of cg8pres{*})=-1 then r8presvp=-3; ** PROXY SAYS CAN SPEAK TO SP
	BUT SP UNABLE TO ANSWER **;
	end;
	r8presvpr=r8presvp;
	if r8presvp=-2 then r8presvpr=.; ** MISSING IF PROXY SAYS CAN'T SPEAK TO SP **;
	else if r8presvp=-3 then r8presvpr=0; ** 0 IF SP UNABLE TO ANSWER **;
	** 7) ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING **;
	r8date_prvp=sum(r8date_sumr,r8presvpr);
	** 8) EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE **;
	r8clock_scorer=cg8dclkdraw;
	if cg8dclkdraw in (-2,-9) then r8clock_scorer=.;
	if cg8dclkdraw in (-3,-4,-7) then r8clock_scorer=0;
	** IMPUTE MEAN SCORE TO PERSONS MISSING A CLOCK **;
	if cg8dclkdraw=-9 and cg8speaktosp=1 then r8clock_scorer=2; ** IF PROXY SAID CAN ASK SP **;
	if cg8dclkdraw=-9 and cg8speaktosp=-1 then r8clock_scorer=3; ** IF SELF RESPONDENT **;
	** 9) MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL **;
	array cg8recall {*} cg8dwrdimmrc cg8dwrddlyrc;
	array r8word_recall {*} r8irecall r8drecall;
	do i=1 to dim(r8word_recall);
	r8word_recall{i}=cg8recall{i};
	if cg8recall{i} in (-2,-1) then r8word_recall{i}=.;
	if cg8recall{i} in (-7,-3) then r8word_recall{i}=0;
	r8wordrecall0_20=sum(of r8word_recall{*});
	end;
	** 10) CREATE COGNITIVE DOMAINS FOR ALL ELIGIBLE **;
	** I.E. PROXY BUT PROXY SAYS CAN ASK SP, NOT FQ ONLY, NOT NH **;
	if 1 < r8clock_scorer <=5 then r8clock65=0;
	if 0 <= r8clock_scorer <=1 then r8clock65=1;
	if 3 < r8wordrecall0_20 <=20 then r8word65=0;
	if 0 <= r8wordrecall0_20 <=3 then r8word65=1;
	if 3 < r8date_prvp <= 8 then r8datena65=0;
	if 0 <= r8date_prvp <= 3 then r8datena65=1;
	** 10) CREATE COGNITIVE DOMAIN SCORE **;
	array r8domains {*} r8clock65 r8word65 r8datena65;
	do i=1 to dim(r8domains);
	r8domain65=sum(of r8domains{*});
	end;
	** 11) UPDATE COGNITIVE CLASSIFICATION **;
	if r8demclas=. and cg8speaktosp in (-1,1) then do;
	if 2 <= r8domain65 <=3 then r8demclas=1; ** PROBABLE DEMENTIA **;
	if r8domain65 =1 then r8demclas=2; ** POSSIBLE DEMENTIA **;
	if r8domain65 =0 then r8demclas=3; ** NO DEMENTIA **;
	format r8demclas demclas. r8ad8_dem ad8dem. r8clock65 clockf. r8word65 wordrecf. r8datena65 dateprf.;
	end;
	Run;

/************************************************
Restrict dataset to community-dwelling and  
residential care (not NH) respondents with 
SP questionnaire							
************************************************/
Data QOLDEM_R1; Set demclas_R1; Where r1dresid in(1,2); Run; *8245 obs to 7609;
Data QOLDEM_R2; Set demclas_R2; Where r2dresid in(1,2); Run; *7075 obs to 5992;
Data QOLDEM_R3; Set demclas_R3; Where r3dresid in(1,2); Run; *5799 obs to 4803;
Data QOLDEM_R4; Set demclas_R4; Where r4dresid in(1,2); Run; *4737 obs to 3941;
Data QOLDEM_R5; Set demclas_R5; Where r5dresid in(1,2); Run; *8334 obs to 7499;
Data QOLDEM_R6; Set demclas_R6; Where r6dresid in(1,2); Run; *7276 obs to 6309;
Data QOLDEM_R7; Set demclas_R7; Where r7dresid in(1,2); Run; *6312 obs to 5454;
Data QOLDEM_R8; Set demclas_R8; Where r8dresid in(1,2); Run; *5547 obs to 4837;
	
/******************************************************
***	Create datasets with Table 1, QOL, and DEM vars ***
******************************************************/

Data QOLDEMvars_R1; Set QOLDEM_R1 
(keep= 	spid r1dgender r1d2intvrage rl1dracehisp el1higstschl r1dresid is1resptype			/*ID, demographics*/
		El1borninus el1dage2us re1dcensdiv is1prxyrelat is1famrrutin hh1proxlivsp	 		/*demographics*/
		Hh1dlvngarrg hh1dhshldnum hh1dhshldchd 						 	 	 				/*demographics*/
		Hc1disescn3 hc1disescn6 hc1disescn8 hc1disescn9 hc1disescn10    		   			/*Clinical demographics*/
		W1anfinwgt0 w1varstrat w1varunit					    							/*Round 1 analytic weight variables*/
		cg1speaktosp cg1ratememry cp1chgthink1 cp1chgthink2 cp1chgthink3 cp1chgthink4		/*Dementia diagnosis report, self-rate memory*/
		Cp1chgthink5 cp1chgthink6 cp1chgthink7 cp1chgthink8 r1ad8_dem 				    	/*AD8 items*/
		Cg1todaydat1 cg1todaydat2 cg1todaydat3 cg1todaydat4 cg1todaydat5			  		/*Today's date test*/
		Cg1presidna1 cg1presidna3 cg1vpname1 cg1vpname3 r1datena65 						  	/*President/VP name test, date + name combined score*/
		Cg1dclkdraw r1clock65 cg1dwrdimmrc cg1dwrddlyrc r1word65 							/*Clock drawing test, immediate, delayed word recall scores*/
		r1ad8_score r1ad8_miss r1clock_scorer r1wordrecall0_20 r1date_prvp					/*AD8 + Cognitive domain scores*/
		R1demclas hc1depresan1 hc1depresan2 hc1depresan3 hc1depresan4						/*Derived dementia variable, depression, anxiety screenings*/
		Hc1health ss1painbothr sc1deathelp sc1dbathhelp sc1dtoilhelp						/*Self-report health, bothered by pain, eating ADL, bathing ADL, toileting ADL*/
		Sc1ddreshelp  mo1dinsdhelp mo1dbedhelp mo1douthelp									/*Dressing ADL, moving inside ADL, Getting out of bed ADL, moving outside ADL*/
		Wb1offelche1 wb1offelche2 wb1offelche3 wb1offelche4				   		 			/*Subjective well-being*/
		Wb1truestme1 wb1truestme2 wb1truestme3 wb1truestme4);								/*Self-realization, living arrangement situation*/
	
	round = 1;
	Run;
	/*WORK.QOLDEMVARS_R1 has 7609 observations and 78 variables*/

Data QOLDEMvars_R5; Set QOLDEM_R5 
(keep=	spid r5dgender r5d2intvrage rl5dracehisp el5higstschl r5dresid is5resptype			/*ID, demographics*/
		el5borninus el5dage2us re5dcensdiv is5prxyrelat is5famrrutin is5proxlivsp	 		/*demographics*/
		hh5dlvngarrg hh5dhshldnum hh5dhshldchd 						 	 					/*demographics*/
		hc5disescn3 hc5disescn6 hc5disescn8 hc5disescn9 hc5disescn10	    				/*Clinical demographics*/
		r5dcontnew w5anfinwgt0 w5varstrat w5varunit					    					/*Round 5 analytic weight variables*/
		cg5speaktosp cg5ratememry cp5chgthink1 cp5chgthink2 cp5chgthink3 cp5chgthink4		/*Dementia diagnosis report, self-rate memory*/
		cp5chgthink5 cp5chgthink6 cp5chgthink7 cp5chgthink8 r5ad8_dem				    	/*AD8 items*/
		cg5todaydat1 cg5todaydat2 cg5todaydat3 cg5todaydat4 cg5todaydat5			  		/*Today's date test*/
		cg5presidna1 cg5presidna3 cg5vpname1 cg5vpname3 r5datena65					     	/*President/VP name test, date + name combined score*/
		cg5dclkdraw r5clock65 cg5dwrdimmrc cg5dwrddlyrc r5word65							/*Clock drawing test, immediate, delayed word recall scores*/
		cp5dad8dem r5ad8_score r5ad8_miss r5clock_scorer r5wordrecall0_20 r5date_prvp		/*Cognitive domain scores*/
		r5demclas hc5depresan1 hc5depresan2 hc5depresan3 hc5depresan4						/*Derived dementia variable, depression, anxiety screenings*/
		hc5health ss5painbothr sc5deathelp sc5dbathhelp sc5dtoilhelp						/*Self-report health, bothered by pain, eating ADL, bathing ADL, toileting ADL*/
		sc5ddreshelp  mo5dinsdhelp mo5dbedhelp mo5douthelp									/*Dressing ADL, moving inside ADL, Getting out of bed ADL, moving outside ADL*/
		wb5offelche1 wb5offelche2 wb5offelche3 wb5offelche4				    				/*Subjective well-being*/
		wb5truestme1 wb5truestme2 wb5truestme3 wb5truestme4);								/*Self-realization, living arrangement situation*/

	round = 5;
	Run;
	/*WORK.QOLDEMVARS_R5 has 7499 observations and 80 variables*/

Data QOLDEMvars_R2; Set QOLDEM_R2 
(keep= 	spid r2dresid r2d2intvrage is2resptype is2prxyrelat		   		   					/*ID, demographics*/		
		is2famrrutin is2proxlivsp hh2dlvngarrg hh2dhshldnum hh2dhshldchd re2dcensdiv		/*demographics*/
		Hc2disescn3 hc2disescn6 hc2disescn8 hc2disescn9 hc2disescn10	    	    		/*Clinical demographics*/
		W2anfinwgt0 w2varstrat w2varunit					    							/*Round 2 analytic weight variables*/
		cg2speaktosp cg2ratememry cp2chgthink1 cp2chgthink2 cp2chgthink3 cp2chgthink4		/*Dementia diagnosis report, self-rate memory*/
		Cp2chgthink5 cp2chgthink6 cp2chgthink7 cp2chgthink8 r2ad8_dem				    	/*AD8 items*/
		Cg2todaydat1 cg2todaydat2 cg2todaydat3 cg2todaydat4 cg2todaydat5			  		/*Today's date test*/
		Cg2presidna1 cg2presidna3 Cg2vpname1 cg2vpname3 r2datena65							/*President/VP name test, date + name combined score*/
		Cg2dclkdraw r2clock65 cg2dwrdimmrc cg2dwrddlyrc r2word65							/*Clock drawing test, immediate, delayed word recall scores*/
		cp2dad8dem r2ad8_score r2ad8_miss r2clock_scorer r2wordrecall0_20 r2date_prvp		/*Cognitive domain scores*/
		R2demclas hc2depresan1 hc2depresan2 hc2depresan3 hc2depresan4						/*Derived dementia variable, depression, anxiety screenings*/
		Hc2health ss2painbothr sc2deathelp sc2dbathhelp sc2dtoilhelp						/*Self-report health, bothered by pain, eating ADL, bathing ADL, toileting ADL*/
		Sc2ddreshelp  mo2dinsdhelp mo2dbedhelp mo2douthelp									/*Dressing ADL, moving inside ADL, Getting out of bed ADL, moving outside ADL*/
		Wb2offelche1 wb2offelche2 wb2offelche3 wb2offelche4				    				/*Subjective well-being*/
		Wb2truestme1 wb2truestme2 wb2truestme3 wb2truestme4);								/*Self-realization, living arrangement situation*/

	round = 2;
	Run;
	/*WORK.QOLDEMVARS_R2 has 5992 observations and 74 variables*/

Data QOLDEMvars_R3; Set QOLDEM_R3 
(keep= 	spid r3dresid r3d2intvrage is3resptype is3prxyrelat		   		   					/*ID, demographics*/		
		is3famrrutin is3proxlivsp hh3dlvngarrg hh3dhshldnum hh3dhshldchd re3dcensdiv		/*demographics*/
		Hc3disescn3 hc3disescn6 hc3disescn8 hc3disescn9 hc3disescn10	    	    		/*Clinical demographics*/
		W3anfinwgt0 w3varstrat w3varunit					    							/*Round 3 analytic weight variables*/
		cg3speaktosp cg3ratememry cp3chgthink1 cp3chgthink2 cp3chgthink3 cp3chgthink4		/*Dementia diagnosis report, self-rate memory*/
		Cp3chgthink5 cp3chgthink6 cp3chgthink7 cp3chgthink8 r3ad8_dem				    	/*AD8 items*/
		Cg3todaydat1 cg3todaydat2 cg3todaydat3 cg3todaydat4 cg3todaydat5			  		/*Today's date test*/
		Cg3presidna1 cg3presidna3 Cg3vpname1 cg3vpname3 r3datena65							/*President/VP name test, date + name combined score*/
		Cg3dclkdraw r3clock65 cg3dwrdimmrc cg3dwrddlyrc r3word65							/*Clock drawing test, immediate, delayed word recall scores*/
		cp3dad8dem r3ad8_score r3ad8_miss r3clock_scorer r3wordrecall0_20 r3date_prvp		/*Cognitive domain scores*/
		R3demclas hc3depresan1 hc3depresan2 hc3depresan3 hc3depresan4						/*Derived dementia variable, depression, anxiety screenings*/
		Hc3health ss3painbothr sc3deathelp sc3dbathhelp sc3dtoilhelp						/*Self-report health, bothered by pain, eating ADL, bathing ADL, toileting ADL*/
		Sc3ddreshelp  mo3dinsdhelp mo3dbedhelp mo3douthelp									/*Dressing ADL, moving inside ADL, Getting out of bed ADL, moving outside ADL*/
		Wb3offelche1 wb3offelche2 wb3offelche3 wb3offelche4				    				/*Subjective well-being*/
		Wb3truestme1 wb3truestme2 wb3truestme3 wb3truestme4);								/*Self-realization, living arrangement situation*/

	round = 3;
	Run;
	/*WORK.QOLDEMVARS_R3 has 4803 observations and 74 variables*/

Data QOLDEMvars_R4; Set QOLDEM_R4 
(keep= 	spid r4dresid r4d2intvrage is4resptype is4prxyrelat		   		   					/*ID, demographics*/		
		is4famrrutin is4proxlivsp hh4dlvngarrg hh4dhshldnum hh4dhshldchd re4dcensdiv		/*demographics*/
		Hc4disescn3 hc4disescn6 hc4disescn8 hc4disescn9 hc4disescn10	    	    		/*Clinical demographics*/
		W4anfinwgt0 w4varstrat w4varunit					    							/*Round 4 analytic weight variables*/
		cg4speaktosp cg4ratememry cp4chgthink1 cp4chgthink2 cp4chgthink3 cp4chgthink4		/*Dementia diagnosis report, self-rate memory*/
		Cp4chgthink5 cp4chgthink6 cp4chgthink7 cp4chgthink8 r4ad8_dem				    	/*AD8 items*/
		Cg4todaydat1 cg4todaydat2 cg4todaydat3 cg4todaydat5 cg4todaydat4			  		/*Today's date test*/
		Cg4presidna1 cg4presidna3 Cg4vpname1 cg4vpname3 r4datena65							/*President/VP name test, date + name combined score*/
		Cg4dclkdraw r4clock65 cg4dwrdimmrc cg4dwrddlyrc r4word65							/*Clock drawing test, immediate, delayed word recall scores*/
		cp4dad8dem r4ad8_score r4ad8_miss r4clock_scorer r4wordrecall0_20 r4date_prvp		/*Cognitive domain scores*/
		R4demclas hc4depresan1 hc4depresan2 hc4depresan3 hc4depresan4						/*Derived dementia variable, depression, anxiety screenings*/
		Hc4health ss4painbothr sc4deathelp sc4dbathhelp sc4dtoilhelp						/*Self-report health, bothered by pain, eating ADL, bathing ADL, toileting ADL*/
		Sc4ddreshelp  mo4dinsdhelp mo4dbedhelp mo4douthelp									/*Dressing ADL, moving inside ADL, Getting out of bed ADL, moving outside ADL*/
		Wb4offelche1 wb4offelche2 wb4offelche3 wb4offelche4				    				/*Subjective well-being*/
		Wb4truestme1 wb4truestme2 wb4truestme3 wb4truestme4);								/*Self-realization, living arrangement situation*/

	round = 4;
	Run;
	/*WORK.QOLDEMVARS_R4 has 3941 observations and 74 variables*/

Data QOLDEMvars_R6; Set QOLDEM_R6 
(keep= 	spid r6dresid r6d2intvrage is6resptype is6prxyrelat		   		   					/*ID, demographics*/		
		is6famrrutin is6proxlivsp hh6dlvngarrg hh6dhshldnum hh6dhshldchd re6dcensdiv		/*demographics*/
		Hc6disescn3 hc6disescn6 hc6disescn8 hc6disescn9 hc6disescn10	    	    		/*Clinical demographics*/
		W6anfinwgt0 w6varstrat w6varunit					    							/*Round 5 analytic weight variables*/
		cg6speaktosp cg6ratememry cp6chgthink1 cp6chgthink2 cp6chgthink3 cp6chgthink4		/*Dementia diagnosis report, self-rate memory*/
		Cp6chgthink5 cp6chgthink6 cp6chgthink7 cp6chgthink8 r6ad8_dem				    	/*AD8 items*/
		Cg6todaydat1 cg6todaydat2 cg6todaydat3 cg6todaydat4 cg6todaydat5		   	  		/*Today's date test*/
		Cg6presidna1 cg6presidna3 Cg6vpname1 cg6vpname3 r6datena65							/*President/VP name test, date + name combined score*/
		Cg6dclkdraw r6clock65 cg6dwrdimmrc cg6dwrddlyrc r6word65							/*Clock drawing test, immediate, delayed word recall scores*/
		cp6dad8dem r6ad8_score r6ad8_miss r6clock_scorer r6wordrecall0_20 r6date_prvp		/*Cognitive domain scores*/
		R6demclas hc6depresan1 hc6depresan2 hc6depresan3 hc6depresan4						/*Derived dementia variable, depression, anxiety screenings*/
		Hc6health ss6painbothr sc6deathelp sc6dbathhelp sc6dtoilhelp						/*Self-report health, bothered by pain, eating ADL, bathing ADL, toileting ADL*/
		Sc6ddreshelp  mo6dinsdhelp mo6dbedhelp mo6douthelp									/*Dressing ADL, moving inside ADL, Getting out of bed ADL, moving outside ADL*/
		Wb6offelche1 wb6offelche2 wb6offelche3 wb6offelche4				    				/*Subjective well-being*/
		Wb6truestme1 wb6truestme2 wb6truestme3 wb6truestme4);								/*Self-realization, living arrangement situation*/

	round = 6;
	Run;
	/*WORK.QOLDEMVARS_R6 has 6309 observations and 74 variables*/

Data QOLDEMvars_R7; Set QOLDEM_R7 
(keep= 	spid r7dresid r7d2intvrage is7resptype is7prxyrelat		   		   					/*ID, demographics*/		
		is7famrrutin is7proxlivsp hh7dlvngarrg hh7dhshldnum hh7dhshldchd re7dcensdiv		/*demographics*/
		Hc7disescn3 hc7disescn6 hc7disescn8 hc7disescn9 hc7disescn10	    	    		/*Clinical demographics*/
		W7anfinwgt0 w7varstrat w7varunit					    							/*Round 5 analytic weight variables*/
		cg7speaktosp cg7ratememry cp7chgthink1 cp7chgthink2 cp7chgthink3 cp7chgthink4		/*Dementia diagnosis report, self-rate memory*/
		Cp7chgthink5 cp7chgthink6 cp7chgthink7 cp7chgthink8 r7ad8_dem				    	/*AD8 items*/
		Cg7todaydat1 cg7todaydat2 cg7todaydat3 cg7todaydat4 cg7todaydat5			  		/*Today's date test*/
		Cg7presidna1 cg7presidna3 Cg7vpname1 cg7vpname3 r7datena65							/*President/VP name test, date + name combined score*/
		Cg7dclkdraw r7clock65 cg7dwrdimmrc cg7dwrddlyrc r7word65							/*Clock drawing test, immediate, delayed word recall scores*/
		cp7dad8dem r7ad8_score r7ad8_miss r7clock_scorer r7wordrecall0_20 r7date_prvp		/*Cognitive domain scores*/
		R7demclas hc7depresan1 hc7depresan2 hc7depresan3 hc7depresan4						/*Derived dementia variable, depression, anxiety screenings*/
		Hc7health ss7painbothr sc7deathelp sc7dbathhelp sc7dtoilhelp						/*Self-report health, bothered by pain, eating ADL, bathing ADL, toileting ADL*/
		Sc7ddreshelp  mo7dinsdhelp mo7dbedhelp mo7douthelp									/*Dressing ADL, moving inside ADL, Getting out of bed ADL, moving outside ADL*/
		Wb7offelche1 wb7offelche2 wb7offelche3 wb7offelche4				   					/*Subjective well-being*/
		Wb7truestme1 wb7truestme2 wb7truestme3 wb7truestme4);								/*Self-realization, living arrangement situation*/

	round = 7;
	Run;
	/*WORK.QOLDEMVARS_R7 has 5454 observations and 74 variables*/

Data QOLDEMvars_R8; Set QOLDEM_R8
(keep= 	spid r8dresid r8d2intvrage is8resptype is8prxyrelat		   		   					/*ID, demographics*/		
		is8famrrutin is8proxlivsp hh8dlvngarrg hh8dhshldnum hh8dhshldchd re8dcensdiv		/*demographics*/
		Hc8disescn3 hc8disescn6 hc8disescn8 hc8disescn9 hc8disescn10	    	    		/*Clinical demographics*/
		W8anfinwgt0 w8varstrat w8varunit					    							/*Round 5 analytic weight variables*/
		cg8speaktosp cg8ratememry cp8chgthink1 cp8chgthink2 cp8chgthink3 cp8chgthink4		/*Dementia diagnosis report, self-rate memory*/
		Cp8chgthink5 cp8chgthink6 cp8chgthink7 cp8chgthink8 r8ad8_dem				    	/*AD8 items*/
		Cg8todaydat1 cg8todaydat2 cg8todaydat3 cg8todaydat4 cg8todaydat5			  		/*Today's date test*/
		Cg8presidna1 cg8presidna3 Cg8vpname1 cg8vpname3 r8datena65							/*President/VP name test, date + name combined score*/
		Cg8dclkdraw r8clock65 cg8dwrdimmrc cg8dwrddlyrc r8word65							/*Clock drawing test, immediate, delayed word recall scores*/
		cp8dad8dem r8ad8_score r8ad8_miss r8clock_scorer r8wordrecall0_20 r8date_prvp		/*Cognitive domain scores*/
		R8demclas hc8depresan1 hc8depresan2 hc8depresan3 hc8depresan4						/*Derived dementia variable, depression, anxiety screenings*/
		Hc8health ss8painbothr sc8deathelp sc8dbathhelp sc8dtoilhelp						/*Self-report health, bothered by pain, eating ADL, bathing ADL, toileting ADL*/
		Sc8ddreshelp  mo8dinsdhelp mo8dbedhelp mo8douthelp									/*Dressing ADL, moving inside ADL, Getting out of bed ADL, moving outside ADL*/
		Wb8offelche1 wb8offelche2 wb8offelche3 wb8offelche4				    				/*Subjective well-being*/
		Wb8truestme1 wb8truestme2 wb8truestme3 wb8truestme4);								/*Self-realization, living arrangement situation*/

	round = 8;
	Run;
	/*WORK.QOLDEMVARS_R8 has 4837 observations and 74 variables*/

/***********************
***	Merge Rounds 1-8 ***
***********************/
Proc sort data=QOLDEMvars_R1; By spid round; Run; /*7609 obs, 78 vars*/
Proc sort data=QOLDEMvars_R2; By spid round; Run; /*5992 obs, 74 vars*/
Proc sort data=QOLDEMvars_R3; By spid round; Run; /*4803 obs, 74 vars*/
Proc sort data=QOLDEMvars_R4; By spid round; Run; /*3941 obs, 74 vars*/
Proc sort data=QOLDEMvars_R5; By spid round; Run; /*7499 obs, 80 vars*/
Proc sort data=QOLDEMvars_R6; By spid round; Run; /*6309 obs, 74 vars*/
Proc sort data=QOLDEMvars_R7; By spid round; Run; /*5454 obs, 74 vars*/
Proc sort data=QOLDEMvars_R8; By spid round; Run; /*4837 obs, 74 vars*/


Data QOLDEM_pooled; Set QOLDEMvars_R1 QOLDEMvars_R2 QOLDEMvars_R3 QOLDEMvars_R4 QOLDEMvars_R5 QOLDEMvars_R6 QOLDEMvars_R7 QOLDEMvars_R8; Run; 
/*WORK.QOLDEM_POOLED has 46444 observations and 588 variables*/


/****************************
*** Merge metro/non-metro *** 
***		derived variable  ***
****************************/

Data NHATS_metnonmet; Set NHATS.nhats_round_1_8_metnonmet;
	format  r1dmetnonmet r2dmetnonmet r3dmetnonmet r4dmetnonmet r5dmetnonmet r6dmetnonmet r7dmetnonmet r8dmetnonmet W00metro.; 
	Run;
	
	Proc sort data=NHATS_metnonmet; By spid; Run;
	Proc sort data=QOLDEM_pooled; By spid round; Run;
	Data NHATS_QOLDEM_pooled; Merge QOLDEM_pooled(in=A) NHATS_metnonmet(in=B); By spid; If A; Run;
	/*WORK.NHATS_pooled has 46444 observations and 596 variables*/

	Proc print data=NHATS_QOLDEM_pooled (obs=100); Var spid round r1demclas r2demclas r3demclas r4demclas r5demclas; Run;

/*********************************************
*** Create indicator vars for tall dataset ***
*********************************************/

Data NHATS_QOLDEM_pooled2; Set NHATS_QOLDEM_pooled;
By spid;
	*Create first observation indicator;
	If first.spid then first_obs = 1;
		Else first_obs = 0;
	*Create last observation indicator;
	If last.spid then last_obs = 1;
		Else last_obs = 0;
	*Create number of observations var;
	count_obs+1;
    if first.spid then count_obs=1;
	where spid ne .;
	Run; 
	/*WORK.NHATS_QOLDEM_POOLED2 has 46444 observations and 599 variables*/

/*******************
*** Save dataset ***
/******************/

Data qol.NHATS_QOLDEM_pooled; Set NHATS_QOLDEM_pooled2; Run;
/*QOL.NHATS_pooled has 46444 observations and 599 variables*/



/************************************************************************
	Part two: retain round variables through all spid observations
************************************************************************/

Data NHATS_pooled; Set qol.NHATS_QOLDEM_pooled; RUn;

Proc sort data=NHATS_pooled; By spid round; Run;

/*demographic, analytic weight, and health history variables*/
data NHATS_pooled1;
array _a{155}r1dresid r2dresid r3dresid r4dresid r5dresid r6dresid r7dresid r8dresid
			r1d2intvrage r2d2intvrage r3d2intvrage r4d2intvrage r5d2intvrage r6d2intvrage r7d2intvrage r8d2intvrage
			re1dcensdiv re2dcensdiv re3dcensdiv re4dcensdiv re5dcensdiv re6dcensdiv re7dcensdiv re8dcensdiv
			W1anfinwgt0 W2anfinwgt0 W3anfinwgt0 W4anfinwgt0 W5anfinwgt0 W6anfinwgt0 W7anfinwgt0 W8anfinwgt0
			w1varstrat w2varstrat w3varstrat w4varstrat w5varstrat w6varstrat w7varstrat w8varstrat
			w1varunit w2varunit w3varunit w4varunit w5varunit w6varunit w7varunit w8varunit	

			r5dcontnew
			r1dgender r5dgender
			rl1dracehisp rl5dracehisp
			el1higstschl el5higstschl
			el1borninus el5borninus
			el1dage2us el5dage2us 

			is1resptype is2resptype is3resptype is4resptype is5resptype is6resptype is7resptype is8resptype 
			is1prxyrelat is2prxyrelat is3prxyrelat is4prxyrelat is5prxyrelat is6prxyrelat is7prxyrelat is8prxyrelat
			is1famrrutin is2famrrutin is3famrrutin is4famrrutin is5famrrutin is6famrrutin is7famrrutin is8famrrutin
			hh1proxlivsp is2proxlivsp is3proxlivsp is4proxlivsp is5proxlivsp is6proxlivsp is7proxlivsp is8proxlivsp
			hh1dlvngarrg hh2dlvngarrg hh3dlvngarrg hh4dlvngarrg hh5dlvngarrg hh6dlvngarrg hh7dlvngarrg hh8dlvngarrg
			hh1dhshldnum hh2dhshldnum hh3dhshldnum hh4dhshldnum hh5dhshldnum hh6dhshldnum hh7dhshldnum hh8dhshldnum
			hh1dhshldchd hh2dhshldchd hh3dhshldchd hh4dhshldchd hh5dhshldchd hh6dhshldchd hh7dhshldchd hh8dhshldchd

			hc1disescn3 hc2disescn3 hc3disescn3 hc4disescn3 hc5disescn3 hc6disescn3 hc7disescn3 hc8disescn3 
			hc1disescn6 hc2disescn6 hc3disescn6 hc4disescn6 hc5disescn6 hc6disescn6 hc7disescn6 hc8disescn6
			hc1disescn8 hc2disescn8 hc3disescn8 hc4disescn8 hc5disescn8 hc6disescn8 hc7disescn8 hc8disescn8
			hc1disescn9 hc2disescn9 hc3disescn9 hc4disescn9 hc5disescn9 hc6disescn9 hc7disescn9 hc8disescn9
			hc1disescn10 hc2disescn10 hc3disescn10 hc4disescn10 hc5disescn10 hc6disescn10 hc7disescn10 hc8disescn10
;
array _b{155};
do until(last.spid);
    set NHATS_pooled; by spid;
    do i = 1 to dim(_a);
        _b{i} = coalesce(_b{i}, _a{i});
        end;
    end;
do until(last.spid);
    set NHATS_pooled; by spid;
    do i = 1 to dim(_a);
        _a{i} = _b{i};
        end;
    output;
    end;
drop i _b:;
run;


/*probable possible dementia algorithm variables + outcome variables*/
data NHATS_pooled2;
array _a{431} cg1ratememry cg2ratememry cg3ratememry cg4ratememry cg5ratememry cg6ratememry cg7ratememry cg8ratememry 
			cg1speaktosp cg2speaktosp cg3speaktosp cg4speaktosp cg5speaktosp cg6speaktosp cg7speaktosp cg8speaktosp
			cp1chgthink1 cp2chgthink1 cp3chgthink1 cp4chgthink1 cp5chgthink1 cp6chgthink1 cp7chgthink1 cp8chgthink1
			cp1chgthink2 cp2chgthink2 cp3chgthink2 cp4chgthink2 cp5chgthink2 cp6chgthink2 cp7chgthink2 cp8chgthink2
			cp1chgthink3 cp2chgthink3 cp3chgthink3 cp4chgthink3 cp5chgthink3 cp6chgthink3 cp7chgthink3 cp8chgthink3
			cp1chgthink4 cp2chgthink4 cp3chgthink4 cp4chgthink4 cp5chgthink4 cp6chgthink4 cp7chgthink4 cp8chgthink4
			Cp1chgthink5 Cp2chgthink5 Cp3chgthink5 Cp4chgthink5	Cp5chgthink5 Cp6chgthink5 Cp7chgthink5 Cp8chgthink5
			cp1chgthink6 cp2chgthink6 cp3chgthink6 cp4chgthink6 cp5chgthink6 cp6chgthink6 cp7chgthink6 cp8chgthink6
			cp1chgthink7 cp2chgthink7 cp3chgthink7 cp4chgthink7 cp5chgthink7 cp6chgthink7 cp7chgthink7 cp8chgthink7
			cp1chgthink8 cp2chgthink8 cp3chgthink8 cp4chgthink8 cp5chgthink8 cp6chgthink8 cp7chgthink8 cp8chgthink8

			cp2dad8dem cp3dad8dem cp4dad8dem cp5dad8dem cp6dad8dem cp7dad8dem cp8dad8dem
			r1ad8_score r2ad8_score r3ad8_score r4ad8_score r5ad8_score r6ad8_score r7ad8_score r8ad8_score
			r1ad8_miss r2ad8_miss r3ad8_miss r4ad8_miss r5ad8_miss r6ad8_miss r7ad8_miss r8ad8_miss
			r1ad8_dem r2ad8_dem r3ad8_dem r4ad8_dem r5ad8_dem r6ad8_dem r7ad8_dem r8ad8_dem
			Cg1todaydat1 Cg2todaydat1 Cg3todaydat1 Cg4todaydat1 Cg5todaydat1 Cg6todaydat1 Cg7todaydat1 Cg8todaydat1 
			cg1todaydat2 cg2todaydat2 cg3todaydat2 cg4todaydat2 cg5todaydat2 cg6todaydat2 cg7todaydat2 cg8todaydat2 
			cg1todaydat3 cg2todaydat3 cg3todaydat3 cg4todaydat3 cg5todaydat3 cg6todaydat3 cg7todaydat3 cg8todaydat3 
			cg1todaydat4 cg2todaydat4 cg3todaydat4 cg4todaydat4 cg5todaydat4 cg6todaydat4 cg7todaydat4 cg8todaydat4
			cg1todaydat5 cg2todaydat5 cg3todaydat5 cg4todaydat5 cg5todaydat5 cg6todaydat5 cg7todaydat5 cg8todaydat5

			Cg1presidna1 Cg2presidna1 Cg3presidna1 Cg4presidna1 Cg5presidna1 Cg6presidna1 Cg7presidna1 Cg8presidna1 
			cg1presidna3 cg2presidna3 cg3presidna3 cg4presidna3 cg5presidna3 cg6presidna3 cg7presidna3 cg8presidna3 
			Cg1vpname1 Cg2vpname1 Cg3vpname1 Cg4vpname1 Cg5vpname1 Cg6vpname1 Cg7vpname1 Cg8vpname1 
			cg1vpname3 cg2vpname3 cg3vpname3 cg4vpname3 cg5vpname3 cg6vpname3 cg7vpname3 cg8vpname3
			r1date_prvp r2date_prvp r3date_prvp r4date_prvp r5date_prvp r6date_prvp r7date_prvp r8date_prvp
			r1datena65 r2datena65 r3datena65 r4datena65 r5datena65 r6datena65 r7datena65 r8datena65
			Cg1dclkdraw Cg2dclkdraw Cg3dclkdraw Cg4dclkdraw Cg5dclkdraw Cg6dclkdraw Cg7dclkdraw Cg8dclkdraw 
			r1clock_scorer r2clock_scorer r3clock_scorer r4clock_scorer r5clock_scorer r6clock_scorer r7clock_scorer r8clock_scorer
			r1clock65 r2clock65 r3clock65 r4clock65 r5clock65 r6clock65 r7clock65 r8clock65

			cg1dwrdimmrc cg2dwrdimmrc cg3dwrdimmrc cg4dwrdimmrc cg5dwrdimmrc cg6dwrdimmrc cg7dwrdimmrc cg8dwrdimmrc
			cg1dwrddlyrc cg2dwrddlyrc cg3dwrddlyrc cg4dwrddlyrc cg5dwrddlyrc cg6dwrddlyrc cg7dwrddlyrc cg8dwrddlyrc
			r1wordrecall0_20 r2wordrecall0_20 r3wordrecall0_20 r4wordrecall0_20 r5wordrecall0_20 r6wordrecall0_20 r7wordrecall0_20 r8wordrecall0_20 
			r1word65 r2word65 r3word65 r4word65 r5word65 r6word65 r7word65 r8word65
			R1demclas R2demclas R3demclas R4demclas R5demclas R6demclas R7demclas R8demclas 

			hc1depresan1 hc2depresan1 hc3depresan1 hc4depresan1 hc5depresan1 hc6depresan1 hc7depresan1 hc8depresan1 
			hc1depresan2 hc2depresan2 hc3depresan2 hc4depresan2 hc5depresan2 hc6depresan2 hc7depresan2 hc8depresan2
			hc1depresan3 hc2depresan3 hc3depresan3 hc4depresan3 hc5depresan3 hc6depresan3 hc7depresan3 hc8depresan3 
			hc1depresan4 hc2depresan4 hc3depresan4 hc4depresan4 hc5depresan4 hc6depresan4 hc7depresan4 hc8depresan4
			Hc1health Hc2health Hc3health Hc4health Hc5health Hc6health Hc7health Hc8health
			ss1painbothr ss2painbothr ss3painbothr ss4painbothr ss5painbothr ss6painbothr ss7painbothr ss8painbothr

			sc1deathelp sc2deathelp sc3deathelp sc4deathelp sc5deathelp sc6deathelp sc7deathelp sc8deathelp
			sc1dbathhelp sc2dbathhelp sc3dbathhelp sc4dbathhelp sc5dbathhelp sc6dbathhelp sc7dbathhelp sc8dbathhelp
			sc1dtoilhelp sc2dtoilhelp sc3dtoilhelp sc4dtoilhelp sc5dtoilhelp sc6dtoilhelp sc7dtoilhelp sc8dtoilhelp
			Sc1ddreshelp Sc2ddreshelp Sc3ddreshelp Sc4ddreshelp Sc5ddreshelp Sc6ddreshelp Sc7ddreshelp Sc8ddreshelp
			mo1dinsdhelp mo2dinsdhelp mo3dinsdhelp mo4dinsdhelp mo5dinsdhelp mo6dinsdhelp mo7dinsdhelp mo8dinsdhelp 
			mo1dbedhelp mo2dbedhelp mo3dbedhelp mo4dbedhelp mo5dbedhelp mo6dbedhelp mo7dbedhelp mo8dbedhelp
			mo1douthelp mo2douthelp mo3douthelp mo4douthelp mo5douthelp mo6douthelp mo7douthelp mo8douthelp

			Wb1offelche1 Wb2offelche1 Wb3offelche1 Wb4offelche1 Wb5offelche1 Wb6offelche1 Wb7offelche1 Wb8offelche1 
			wb1offelche2 wb2offelche2 wb3offelche2 wb4offelche2 wb5offelche2 wb6offelche2 wb7offelche2 wb8offelche2
			wb1offelche3 wb2offelche3 wb3offelche3 wb4offelche3 wb5offelche3 wb6offelche3 wb7offelche3 wb8offelche3 
			wb1offelche4 wb2offelche4 wb3offelche4 wb4offelche4 wb5offelche4 wb6offelche4 wb7offelche4 wb8offelche4
			Wb1truestme1 Wb2truestme1 Wb3truestme1 Wb4truestme1 Wb5truestme1 Wb6truestme1 Wb7truestme1 Wb8truestme1 
			wb1truestme2 wb2truestme2 wb3truestme2 wb4truestme2 wb5truestme2 wb6truestme2 wb7truestme2 wb8truestme2 
			wb1truestme3 wb2truestme3 wb3truestme3 wb4truestme3 wb5truestme3 wb6truestme3 wb7truestme3 wb8truestme3
			wb1truestme4 wb2truestme4 wb3truestme4 wb4truestme4 wb5truestme4 wb6truestme4 wb7truestme4 wb8truestme4

;
array _b{431};
do until(last.spid);
    set NHATS_pooled1; by spid;
    do i = 1 to dim(_a);
        _b{i} = coalesce(_b{i}, _a{i});
        end;
    end;
do until(last.spid);
    set NHATS_pooled1; by spid;
    do i = 1 to dim(_a);
        _a{i} = _b{i};
        end;
    output;
    end;
drop i _b:;
run;


/******************************************************
	Part three: create cross-wave variables 
******************************************************/


Proc sort data=NHATS_pooled2; By spid round; Run;

/*Demographics*/
Data NHATS_pooled3; Set NHATS_pooled2;

	/*RESIDENTIAL CARE STATUS*/
	If round = 1 and r1dresid ne . then resid = r1dresid;
		Else If round = 2 and r2dresid ne . then resid = r2dresid;
		Else If round = 3 and r3dresid ne . then resid = r3dresid;
		Else If round = 4 and r4dresid ne . then resid = r4dresid;
		Else If round = 5 and r5dresid ne . then resid = r5dresid;
		Else If round = 6 and r6dresid ne . then resid = r6dresid;
		Else If round = 7 and r7dresid ne . then resid = r7dresid;
		Else If round = 8 and r8dresid ne . then resid = r8dresid;
		Else resid = .;
	/*SP CAT AGE AT INTVW*/
	If round = 1 and r1d2intvrage ne . then intvrage = r1d2intvrage;
		Else If round = 2 and r2d2intvrage ne . then intvrage = r2d2intvrage;
		Else If round = 3 and r3d2intvrage ne . then intvrage = r3d2intvrage;
		Else If round = 4 and r4d2intvrage ne . then intvrage = r4d2intvrage;
		Else If round = 5 and r5d2intvrage ne . then intvrage = r5d2intvrage;
		Else If round = 6 and r6d2intvrage ne . then intvrage = r6d2intvrage;
		Else If round = 7 and r7d2intvrage ne . then intvrage = r7d2intvrage;
		Else If round = 8 and r8d2intvrage ne . then intvrage = r8d2intvrage;
		Else intvrage = .;
	/*SP CENSUS DIVISION*/
	If round = 1 and re1dcensdiv  ne . then censdiv = re1dcensdiv;
		Else If round = 2 and re2dcensdiv ne . then censdiv = re2dcensdiv;
		Else If round = 3 and re3dcensdiv ne . then censdiv = re3dcensdiv;
		Else If round = 4 and re4dcensdiv ne . then censdiv = re4dcensdiv;
		Else If round = 5 and re5dcensdiv ne . then censdiv = re5dcensdiv;
		Else If round = 6 and re6dcensdiv ne . then censdiv = re6dcensdiv;
		Else If round = 7 and re7dcensdiv ne . then censdiv = re7dcensdiv;
		Else If round = 8 and re8dcensdiv ne . then censdiv = re8dcensdiv;
			Else censdiv = .;
	/*SP RESIDING IN METROPOLITAN OR NON-METROPOLITAN COUNTY*/
	If round = 1 and r1dmetnonmet ne . then metnonmet = r1dmetnonmet;
		Else If round = 2 and r2dmetnonmet ne . then metnonmet = r2dmetnonmet;
		Else If round = 3 and r3dmetnonmet ne . then metnonmet = r3dmetnonmet;
		Else If round = 4 and r4dmetnonmet ne . then metnonmet = r4dmetnonmet;
		Else If round = 5 and r5dmetnonmet ne . then metnonmet = r5dmetnonmet;
		Else If round = 6 and r6dmetnonmet ne . then metnonmet = r6dmetnonmet;
		Else If round = 7 and r7dmetnonmet ne . then metnonmet = r7dmetnonmet;
		Else If round = 8 and r8dmetnonmet ne . then metnonmet = r8dmetnonmet;
			Else metnonmet = .;
	/*TYPE OF RESPONDENT*/
	If round = 1 and is1resptype  ne . then resptype = is1resptype;
		Else If round = 2 and is2resptype ne . then resptype = is2resptype;
		Else If round = 3 and is3resptype ne . then resptype = is3resptype;
		Else If round = 4 and is4resptype ne . then resptype = is4resptype;
		Else If round = 5 and is5resptype ne . then resptype = is5resptype;
		Else If round = 6 and is6resptype ne . then resptype = is6resptype;
		Else If round = 7 and is7resptype ne . then resptype = is7resptype;
		Else If round = 8 and is8resptype ne . then resptype = is8resptype;
			Else resptype = .;
	/*PROXY RELATIONSHIP TO SP*/
	If round = 1 and is1prxyrelat  ne . then prxyrelat = is1prxyrelat;
		Else If round = 2 and is2prxyrelat ne . then prxyrelat = is2prxyrelat;
		Else If round = 3 and is3prxyrelat ne . then prxyrelat = is3prxyrelat;
		Else If round = 4 and is4prxyrelat ne . then prxyrelat = is4prxyrelat;
		Else If round = 5 and is5prxyrelat ne . then prxyrelat = is5prxyrelat;
		Else If round = 6 and is6prxyrelat ne . then prxyrelat = is6prxyrelat;
		Else If round = 7 and is7prxyrelat ne . then prxyrelat = is7prxyrelat;
		Else If round = 8 and is8prxyrelat ne . then prxyrelat = is8prxyrelat;
			Else prxyrelat = .; 
	/*FAMILRTY SP DAILY ROUTINE*/
	If round = 1 and is1famrrutin  ne . then famrrutin = is1famrrutin;
		Else If round = 2 and is2famrrutin ne . then famrrutin = is2famrrutin;
		Else If round = 3 and is3famrrutin ne . then famrrutin = is3famrrutin;
		Else If round = 4 and is4famrrutin ne . then famrrutin = is4famrrutin;
		Else If round = 5 and is5famrrutin ne . then famrrutin = is5famrrutin;
		Else If round = 6 and is6famrrutin ne . then famrrutin = is6famrrutin;
		Else If round = 7 and is7famrrutin ne . then famrrutin = is7famrrutin;
		Else If round = 8 and is8famrrutin ne . then famrrutin = is8famrrutin;
			Else famrrutin = .;
	/*PROXY LIVES W SP*/
	If round = 1 and hh1proxlivsp  ne . then proxlivsp = hh1proxlivsp;
		Else If round = 2 and is2proxlivsp ne . then proxlivsp = is2proxlivsp;
		Else If round = 3 and is3proxlivsp ne . then proxlivsp = is3proxlivsp;
		Else If round = 4 and is4proxlivsp ne . then proxlivsp = is4proxlivsp;
		Else If round = 5 and is5proxlivsp ne . then proxlivsp = is5proxlivsp;
		Else If round = 6 and is6proxlivsp ne . then proxlivsp = is6proxlivsp;
		Else If round = 7 and is7proxlivsp ne . then proxlivsp = is7proxlivsp;
		Else If round = 8 and is8proxlivsp ne . then proxlivsp = is8proxlivsp;
			Else proxlivsp = .;
	/*LIVING ARRANGEMENT*/
	If round = 1 and hh1dlvngarrg  ne . then lvngarrg = hh1dlvngarrg;
		Else If round = 2 and hh2dlvngarrg ne . then lvngarrg = hh2dlvngarrg;
		Else If round = 3 and hh3dlvngarrg ne . then lvngarrg = hh3dlvngarrg;
		Else If round = 4 and hh4dlvngarrg ne . then lvngarrg = hh4dlvngarrg;
		Else If round = 5 and hh5dlvngarrg ne . then lvngarrg = hh5dlvngarrg;
		Else If round = 6 and hh6dlvngarrg ne . then lvngarrg = hh6dlvngarrg;
		Else If round = 7 and hh7dlvngarrg ne . then lvngarrg = hh7dlvngarrg;
		Else If round = 8 and hh8dlvngarrg ne . then lvngarrg = hh8dlvngarrg;
			Else lvngarrg = .;
	/*TOTAL NUMBER IN HOUSEHOLD*/
	If round = 1 and hh1dhshldnum  ne . then hshldnum = hh1dhshldnum;
		Else If round = 2 and hh2dhshldnum ne . then hshldnum = hh2dhshldnum;
		Else If round = 3 and hh3dhshldnum ne . then hshldnum = hh3dhshldnum;
		Else If round = 4 and hh4dhshldnum ne . then hshldnum = hh4dhshldnum;
		Else If round = 5 and hh5dhshldnum ne . then hshldnum = hh5dhshldnum;
		Else If round = 6 and hh6dhshldnum ne . then hshldnum = hh6dhshldnum;
		Else If round = 7 and hh7dhshldnum ne . then hshldnum = hh7dhshldnum;
		Else If round = 8 and hh8dhshldnum ne . then hshldnum = hh8dhshldnum;
			Else hshldnum = .;
	/*TOTAL CHILDREN IN HOUSEHOLD*/
	If round = 1 and hh1dhshldchd  ne . then hshldchd = hh1dhshldchd;
		Else If round = 2 and hh2dhshldchd ne . then hshldchd = hh2dhshldchd;
		Else If round = 3 and hh3dhshldchd ne . then hshldchd = hh3dhshldchd;
		Else If round = 4 and hh4dhshldchd ne . then hshldchd = hh4dhshldchd;
		Else If round = 5 and hh5dhshldchd ne . then hshldchd = hh5dhshldchd;
		Else If round = 6 and hh6dhshldchd ne . then hshldchd = hh6dhshldchd;
		Else If round = 7 and hh7dhshldchd ne . then hshldchd = hh7dhshldchd;
		Else If round = 8 and hh8dhshldchd ne . then hshldchd = hh8dhshldchd;
			Else hshldchd = .;
	/*SP HAS HIGH BLOOD PRESSURE*/
	If round = 1 and hc1disescn3  ne . then disescn3 = hc1disescn3;
		Else If round = 2 and hc2disescn3 ne . then disescn3 = hc2disescn3;
		Else If round = 3 and hc3disescn3 ne . then disescn3 = hc3disescn3;
		Else If round = 4 and hc4disescn3 ne . then disescn3 = hc4disescn3;
		Else If round = 5 and hc5disescn3 ne . then disescn3 = hc5disescn3;
		Else If round = 6 and hc6disescn3 ne . then disescn3 = hc6disescn3;
		Else If round = 7 and hc7disescn3 ne . then disescn3 = hc7disescn3;
		Else If round = 8 and hc8disescn3 ne . then disescn3 = hc8disescn3;
			Else disescn3 = .;
	/*SP HAS DIABETES*/
	If round = 1 and hc1disescn6  ne . then disescn6 = hc1disescn6;
		Else If round = 2 and hc2disescn6 ne . then disescn6 = hc2disescn6;
		Else If round = 3 and hc3disescn6 ne . then disescn6 = hc3disescn6;
		Else If round = 4 and hc4disescn6 ne . then disescn6 = hc4disescn6;
		Else If round = 5 and hc5disescn6 ne . then disescn6 = hc5disescn6;
		Else If round = 6 and hc6disescn6 ne . then disescn6 = hc6disescn6;
		Else If round = 7 and hc7disescn6 ne . then disescn6 = hc7disescn6;
		Else If round = 8 and hc8disescn6 ne . then disescn6 = hc8disescn6;
			Else disescn6 = .;
	/*SP HAD STROKE*/
	If round = 1 and hc1disescn8  ne . then disescn8 = hc1disescn8;
		Else If round = 2 and hc2disescn8 ne . then disescn8 = hc2disescn8;
		Else If round = 3 and hc3disescn8 ne . then disescn8 = hc3disescn8;
		Else If round = 4 and hc4disescn8 ne . then disescn8 = hc4disescn8;
		Else If round = 5 and hc5disescn8 ne . then disescn8 = hc5disescn8;
		Else If round = 6 and hc6disescn8 ne . then disescn8 = hc6disescn8;
		Else If round = 7 and hc7disescn8 ne . then disescn8 = hc7disescn8;
		Else If round = 8 and hc8disescn8 ne . then disescn8 = hc8disescn8;
			Else disescn8 = .;
	/*SP HAS DEMENTIA OR ALZH*/
	If round = 1 and hc1disescn9  ne . then disescn9 = hc1disescn9;
		Else If round = 2 and hc2disescn9 ne . then disescn9 = hc2disescn9;
		Else If round = 3 and hc3disescn9 ne . then disescn9 = hc3disescn9;
		Else If round = 4 and hc4disescn9 ne . then disescn9 = hc4disescn9;
		Else If round = 5 and hc5disescn9 ne . then disescn9 = hc5disescn9;
		Else If round = 6 and hc6disescn9 ne . then disescn9 = hc6disescn9;
		Else If round = 7 and hc7disescn9 ne . then disescn9 = hc7disescn9;
		Else If round = 8 and hc8disescn9 ne . then disescn9 = hc8disescn9;
			Else disescn9 = .;
	/*SP HAS CANCER*/
	If round = 1 and hc1disescn10  ne . then disescn10 = hc1disescn10;
		Else If round = 2 and hc2disescn10 ne . then disescn10 = hc2disescn10;
		Else If round = 3 and hc3disescn10 ne . then disescn10 = hc3disescn10;
		Else If round = 4 and hc4disescn10 ne . then disescn10 = hc4disescn10;
		Else If round = 5 and hc5disescn10 ne . then disescn10 = hc5disescn10;
		Else If round = 6 and hc6disescn10 ne . then disescn10 = hc6disescn10;
		Else If round = 7 and hc7disescn10 ne . then disescn10 = hc7disescn10;
		Else If round = 8 and hc8disescn10 ne . then disescn10 = hc8disescn10;
			Else disescn10 = .;
	/*Analytic Weight*/
	If round = 1 and W1anfinwgt0 ne . then anfinwgt0 = W1anfinwgt0;
		Else If round = 2 and W2anfinwgt0 ne . then anfinwgt0 = W2anfinwgt0;
		Else If round = 3 and W3anfinwgt0 ne . then anfinwgt0 = W3anfinwgt0;
		Else If round = 4 and W4anfinwgt0 ne . then anfinwgt0 = W4anfinwgt0;
		Else If round = 5 and W5anfinwgt0 ne . then anfinwgt0 = W5anfinwgt0;
		Else If round = 6 and W6anfinwgt0 ne . then anfinwgt0 = W6anfinwgt0;
		Else If round = 7 and W7anfinwgt0 ne . then anfinwgt0 = W7anfinwgt0;
		Else If round = 8 and W8anfinwgt0 ne . then anfinwgt0 = W8anfinwgt0;
			Else anfinwgt0 = .;
	/*Strata Var*/
	If round = 1 and w1varstrat ne . then varstrat = w1varstrat;
		Else If round = 2 and w2varstrat ne . then varstrat = w2varstrat;
		Else If round = 3 and w3varstrat ne . then varstrat = w3varstrat;
		Else If round = 4 and w4varstrat ne . then varstrat = w4varstrat;
		Else If round = 5 and w5varstrat ne . then varstrat = w5varstrat;
		Else If round = 6 and w6varstrat ne . then varstrat = w6varstrat;
		Else If round = 7 and w7varstrat ne . then varstrat = w7varstrat;
		Else If round = 8 and w8varstrat ne . then varstrat = w8varstrat;
			Else varstrat = .;
	/*Cluster Var*/
	If round = 1 and w1varunit ne . then varunit = w1varunit;
		Else If round = 2 and w2varunit ne . then varunit = w2varunit;
		Else If round = 3 and w3varunit ne . then varunit = w3varunit;
		Else If round = 4 and w4varunit ne . then varunit = w4varunit;
		Else If round = 5 and w5varunit ne . then varunit = w5varunit;
		Else If round = 6 and w6varunit ne . then varunit = w6varunit;
		Else If round = 7 and w7varunit ne . then varunit = w7varunit;
		Else If round = 8 and w8varunit ne . then varunit = w8varunit;
			Else varunit = .;
	/*Baseline analytic weight vars*/
	If first_obs = 1 and W1anfinwgt0 ne . then bl_anfinwgt0 = W1anfinwgt0;
		Else if first_obs = 1 and W5anfinwgt0 ne . then bl_anfinwgt0 = W5anfinwgt0;

	If first_obs = 1 and w1varstrat ne . then bl_varstrat = w1varstrat;
		Else if first_obs = 1 and w5varstrat ne . then bl_varstrat = w5varstrat;
			 
	If first_obs = 1 and w1varunit ne . then bl_varunit = w1varunit;
		Else if first_obs = 1 and w5varunit ne . then bl_varunit = w5varunit;

	Retain bl_anfinwgt0 bl_varstrat bl_varunit;
Run;

/*Prob/poss dementia algorithm and QoL variables*/
Data NHATS_pooled4; Set NHATS_pooled3;

	/*RATE YOUR MEMORY*/
	If round = 1 and cg1ratememry ne . then ratememry = cg1ratememry;
		Else If round = 2 and cg2ratememry ne . then ratememry = cg2ratememry;
		Else If round = 3 and cg3ratememry ne . then ratememry = cg3ratememry;
		Else If round = 4 and cg4ratememry ne . then ratememry = cg4ratememry;
		Else If round = 5 and cg5ratememry ne . then ratememry = cg5ratememry;
		Else If round = 6 and cg6ratememry ne . then ratememry = cg6ratememry;
		Else If round = 7 and cg7ratememry ne . then ratememry = cg7ratememry;
		Else If round = 8 and cg8ratememry ne . then ratememry = cg8ratememry;
		Else ratememry = .;
	/*CG SPEAK TO SP*/
	If round = 1 and cg1speaktosp ne . then speaktosp = cg1speaktosp;
		Else If round = 2 and cg2speaktosp ne . then speaktosp = cg2speaktosp;
		Else If round = 3 and cg3speaktosp ne . then speaktosp = cg3speaktosp;
		Else If round = 4 and cg4speaktosp ne . then speaktosp = cg4speaktosp;
		Else If round = 5 and cg5speaktosp ne . then speaktosp = cg5speaktosp;
		Else If round = 6 and cg6speaktosp ne . then speaktosp = cg6speaktosp;
		Else If round = 7 and cg7speaktosp ne . then speaktosp = cg7speaktosp;
		Else If round = 8 and cg8speaktosp ne . then speaktosp = cg8speaktosp;
		Else speaktosp = .;
	/*DIFF REMEMBER DATE*/
	If round = 1 and cp1chgthink1 ne . then chgthink1 = cp1chgthink1;
		Else If round = 2 and cp2chgthink1 ne . then chgthink1 = cp2chgthink1;
		Else If round = 3 and cp3chgthink1 ne . then chgthink1 = cp3chgthink1;
		Else If round = 4 and cp4chgthink1 ne . then chgthink1 = cp4chgthink1;
		Else If round = 5 and cp5chgthink1 ne . then chgthink1 = cp5chgthink1;
		Else If round = 6 and cp6chgthink1 ne . then chgthink1 = cp6chgthink1;
		Else If round = 7 and cp7chgthink1 ne . then chgthink1 = cp7chgthink1;
		Else If round = 8 and cp8chgthink1 ne . then chgthink1 = cp8chgthink1;
		Else chgthink1 = .;
	/*SP REPEATS SELF*/	
	If round = 1 and cp1chgthink2 ne . then chgthink2 = cp1chgthink2;
		Else If round = 2 and cp2chgthink2 ne . then chgthink2 = cp2chgthink2;
		Else If round = 3 and cp3chgthink2 ne . then chgthink2 = cp3chgthink2;
		Else If round = 4 and cp4chgthink2 ne . then chgthink2 = cp4chgthink2;
		Else If round = 5 and cp5chgthink2 ne . then chgthink2 = cp5chgthink2;
		Else If round = 6 and cp6chgthink2 ne . then chgthink2 = cp6chgthink2;
		Else If round = 7 and cp7chgthink2 ne . then chgthink2 = cp7chgthink2;
		Else If round = 8 and cp8chgthink2 ne . then chgthink2 = cp8chgthink2;
		Else chgthink2 = .;
	/*SP DIFF REMEMBER APPT*/
	If round = 1 and cp1chgthink3 ne . then chgthink3 = cp1chgthink3;
		Else If round = 2 and cp2chgthink3 ne . then chgthink3 = cp2chgthink3;
		Else If round = 3 and cp3chgthink3 ne . then chgthink3 = cp3chgthink3;
		Else If round = 4 and cp4chgthink3 ne . then chgthink3 = cp4chgthink3;
		Else If round = 5 and cp5chgthink3 ne . then chgthink3 = cp5chgthink3;
		Else If round = 6 and cp6chgthink3 ne . then chgthink3 = cp6chgthink3;
		Else If round = 7 and cp7chgthink3 ne . then chgthink3 = cp7chgthink3;
		Else If round = 8 and cp8chgthink3 ne . then chgthink3 = cp8chgthink3;
		Else chgthink3 = .; 
	/*SP CHNG INTRST ACT HOB*/
	If round = 1 and cp1chgthink4 ne . then chgthink4 = cp1chgthink4;
		Else If round = 2 and cp2chgthink4 ne . then chgthink4 = cp2chgthink4;
		Else If round = 3 and cp3chgthink4 ne . then chgthink4 = cp3chgthink4;
		Else If round = 4 and cp4chgthink4 ne . then chgthink4 = cp4chgthink4;
		Else If round = 5 and cp5chgthink4 ne . then chgthink4 = cp5chgthink4;
		Else If round = 6 and cp6chgthink4 ne . then chgthink4 = cp6chgthink4;
		Else If round = 7 and cp7chgthink4 ne . then chgthink4 = cp7chgthink4;
		Else If round = 8 and cp8chgthink4 ne . then chgthink4 = cp8chgthink4;
		Else chgthink4 = .;
	/*SP DIFF WITH MONEY MGMT*/
	If round = 1 and cp1chgthink5 ne . then chgthink5 = cp1chgthink5;
		Else If round = 2 and cp2chgthink5 ne . then chgthink5 = cp2chgthink5;
		Else If round = 3 and cp3chgthink5 ne . then chgthink5 = cp3chgthink5;
		Else If round = 4 and cp4chgthink5 ne . then chgthink5 = cp4chgthink5;
		Else If round = 5 and cp5chgthink5 ne . then chgthink5 = cp5chgthink5;
		Else If round = 6 and cp6chgthink5 ne . then chgthink5 = cp6chgthink5;
		Else If round = 7 and cp7chgthink5 ne . then chgthink5 = cp7chgthink5;
		Else If round = 8 and cp8chgthink5 ne . then chgthink5 = cp8chgthink5;
		Else chgthink5 = .;
	/*SP DIFF LEARNG USE TOOL*/
	If round = 1 and cp1chgthink6 ne . then chgthink6 = cp1chgthink6;
		Else If round = 2 and cp2chgthink6 ne . then chgthink6 = cp2chgthink6;
		Else If round = 3 and cp3chgthink6 ne . then chgthink6 = cp3chgthink6;
		Else If round = 4 and cp4chgthink6 ne . then chgthink6 = cp4chgthink6;
		Else If round = 5 and cp5chgthink6 ne . then chgthink6 = cp5chgthink6;
		Else If round = 6 and cp6chgthink6 ne . then chgthink6 = cp6chgthink6;
		Else If round = 7 and cp7chgthink6 ne . then chgthink6 = cp7chgthink6;
		Else If round = 8 and cp8chgthink6 ne . then chgthink6 = cp8chgthink6;
		Else chgthink6 = .;
	/*SP PRBLMS WITH JDGMNT*/
	If round = 1 and cp1chgthink7 ne . then chgthink7 = cp1chgthink7;
		Else If round = 2 and cp2chgthink7 ne . then chgthink7 = cp2chgthink7;
		Else If round = 3 and cp3chgthink7 ne . then chgthink7 = cp3chgthink7;
		Else If round = 4 and cp4chgthink7 ne . then chgthink7 = cp4chgthink7;
		Else If round = 5 and cp5chgthink7 ne . then chgthink7 = cp5chgthink7;
		Else If round = 6 and cp6chgthink7 ne . then chgthink7 = cp6chgthink7;
		Else If round = 7 and cp7chgthink7 ne . then chgthink7 = cp7chgthink7;
		Else If round = 8 and cp8chgthink7 ne . then chgthink7 = cp8chgthink7;
		Else chgthink7 = .;
	/*SP DLY PRBLMS W THNK MEM*/	 
	If round = 1 and cp1chgthink8 ne . then chgthink8 = cp1chgthink8;
		Else If round = 2 and cp2chgthink8 ne . then chgthink8 = cp2chgthink8;
		Else If round = 3 and cp3chgthink8 ne . then chgthink8 = cp3chgthink8;
		Else If round = 4 and cp4chgthink8 ne . then chgthink8 = cp4chgthink8;
		Else If round = 5 and cp5chgthink8 ne . then chgthink8 = cp5chgthink8;
		Else If round = 6 and cp6chgthink8 ne . then chgthink8 = cp6chgthink8;
		Else If round = 7 and cp7chgthink8 ne . then chgthink8 = cp7chgthink8;
		Else If round = 8 and cp8chgthink8 ne . then chgthink8 = cp8chgthink8;
		Else chgthink8 = .;
	/*DEMENTIA REPORTED IN PRIOR AD8*/
	If round = 2 and cp2dad8dem ne . then ad8_proxydem = cp2dad8dem;
		Else If round = 3 and cp3dad8dem ne . then ad8_proxydem = cp3dad8dem;
		Else If round = 4 and cp4dad8dem ne . then ad8_proxydem = cp4dad8dem;
		Else If round = 5 and cp5dad8dem ne . then ad8_proxydem = cp5dad8dem;
		Else If round = 6 and cp6dad8dem ne . then ad8_proxydem = cp6dad8dem;
		Else If round = 7 and cp7dad8dem ne . then ad8_proxydem = cp7dad8dem;
		Else If round = 8 and cp8dad8dem ne . then ad8_proxydem = cp8dad8dem;
		Else ad8_proxydem = .;
	/*AD8 Score*/
	If round = 1 and r1ad8_score ne . then ad8_score = r1ad8_score;
		Else If round = 2 and r2ad8_score ne . then ad8_score = r2ad8_score;
		Else If round = 3 and r3ad8_score ne . then ad8_score = r3ad8_score;
		Else If round = 4 and r4ad8_score ne . then ad8_score = r4ad8_score;
		Else If round = 5 and r5ad8_score ne . then ad8_score = r5ad8_score;
		Else If round = 6 and r6ad8_score ne . then ad8_score = r6ad8_score;
		Else If round = 7 and r7ad8_score ne . then ad8_score = r7ad8_score;
		Else If round = 8 and r8ad8_score ne . then ad8_score = r8ad8_score;
		Else ad8_score = .;
	/*AD8 Missing*/
	If round = 1 and r1ad8_miss ne . then ad8_miss = r1ad8_miss;
		Else If round = 2 and r2ad8_miss ne . then ad8_miss = r2ad8_miss;
		Else If round = 3 and r3ad8_miss ne . then ad8_miss = r3ad8_miss;
		Else If round = 4 and r4ad8_miss ne . then ad8_miss = r4ad8_miss;
		Else If round = 5 and r5ad8_miss ne . then ad8_miss = r5ad8_miss;
		Else If round = 6 and r6ad8_miss ne . then ad8_miss = r6ad8_miss;
		Else If round = 7 and r7ad8_miss ne . then ad8_miss = r7ad8_miss;
		Else If round = 8 and r8ad8_miss ne . then ad8_miss = r8ad8_miss;
		Else ad8_miss = .;
	/*R1 	AD8 DEMENTIA CLASS 1 = 1:Probable 					2 = 2:Possible 
	  R2-R8 AD8 DEMENTIA CLASS	1 = 1:Meets dementia criteria	2 = 2:Does not meet dementia criteria*/
	If round = 1 and r1ad8_dem ne . then ad8_dem = r1ad8_dem;
		Else If round = 2 and r2ad8_dem ne . then ad8_dem = r2ad8_dem;
		Else If round = 3 and r3ad8_dem ne . then ad8_dem = r3ad8_dem;
		Else If round = 4 and r4ad8_dem ne . then ad8_dem = r4ad8_dem;
		Else If round = 5 and r5ad8_dem ne . then ad8_dem = r5ad8_dem;
		Else If round = 6 and r6ad8_dem ne . then ad8_dem = r6ad8_dem;
		Else If round = 7 and r7ad8_dem ne . then ad8_dem = r7ad8_dem;
		Else If round = 8 and r8ad8_dem ne . then ad8_dem = r8ad8_dem;
		Else ad8_dem = .;
	/*TODAY'S DATE CORRCT MNTH*/
	If round = 1 and Cg1todaydat1 ne . then todaydat1 = Cg1todaydat1;
		Else If round = 2 and Cg2todaydat1 ne . then todaydat1 = Cg2todaydat1;
		Else If round = 3 and Cg3todaydat1 ne . then todaydat1 = Cg3todaydat1;
		Else If round = 4 and Cg4todaydat1 ne . then todaydat1 = Cg4todaydat1;
		Else If round = 5 and Cg5todaydat1 ne . then todaydat1 = Cg5todaydat1;
		Else If round = 6 and Cg6todaydat1 ne . then todaydat1 = Cg6todaydat1;
		Else If round = 7 and Cg7todaydat1 ne . then todaydat1 = Cg7todaydat1;
		Else If round = 8 and Cg8todaydat1 ne . then todaydat1 = Cg8todaydat1;
		Else todaydat1 = .;
	/*TODAY'S DATE CORRECT DAY*/
	If round = 1 and Cg1todaydat2 ne . then todaydat2 = Cg1todaydat2;
		Else If round = 2 and Cg2todaydat2 ne . then todaydat2 = Cg2todaydat2;
		Else If round = 3 and Cg3todaydat2 ne . then todaydat2 = Cg3todaydat2;
		Else If round = 4 and Cg4todaydat2 ne . then todaydat2 = Cg4todaydat2;
		Else If round = 5 and Cg5todaydat2 ne . then todaydat2 = Cg5todaydat2;
		Else If round = 6 and Cg6todaydat2 ne . then todaydat2 = Cg6todaydat2;
		Else If round = 7 and Cg7todaydat2 ne . then todaydat2 = Cg7todaydat2;
		Else If round = 8 and Cg8todaydat2 ne . then todaydat2 = Cg8todaydat2;
		Else todaydat2 = .;
	/*TODAY'S DATE CORRECT YR*/
	If round = 1 and Cg1todaydat3 ne . then todaydat3 = Cg1todaydat3;
		Else If round = 2 and Cg2todaydat3 ne . then todaydat3 = Cg2todaydat3;
		Else If round = 3 and Cg3todaydat3 ne . then todaydat3 = Cg3todaydat3;
		Else If round = 4 and Cg4todaydat3 ne . then todaydat3 = Cg4todaydat3;
		Else If round = 5 and Cg5todaydat3 ne . then todaydat3 = Cg5todaydat3;
		Else If round = 6 and Cg6todaydat3 ne . then todaydat3 = Cg6todaydat3;
		Else If round = 7 and Cg7todaydat3 ne . then todaydat3 = Cg7todaydat3;
		Else If round = 8 and Cg8todaydat3 ne . then todaydat3 = Cg8todaydat3;
		Else todaydat3 = .;
	/*TODAY'S DATE CORRCT DOW - cg#todaydat4, cg4todaydat5*/
	If round = 1 and Cg1todaydat4 ne . then todaydat4 = Cg1todaydat4;
		Else If round = 2 and Cg2todaydat4 ne . then todaydat4 = Cg2todaydat4;
		Else If round = 3 and Cg3todaydat4 ne . then todaydat4 = Cg3todaydat4;
		Else If round = 4 and Cg4todaydat5 ne . then todaydat4 = Cg4todaydat5;
		Else If round = 5 and Cg5todaydat4 ne . then todaydat4 = Cg5todaydat4;
		Else If round = 6 and Cg6todaydat4 ne . then todaydat4 = Cg6todaydat4;
		Else If round = 7 and Cg7todaydat4 ne . then todaydat4 = Cg7todaydat4;
		Else If round = 8 and Cg8todaydat4 ne . then todaydat4 = Cg8todaydat4;
		Else todaydat4 = .;
	/*USED AID FOR MTH DAY YEAR - cg#todaydat5 cg4todaydat4*/
	If round = 1 and Cg1todaydat5 ne . then todaydat5 = Cg1todaydat5;
		Else If round = 2 and Cg2todaydat5 ne . then todaydat5 = Cg2todaydat5;
		Else If round = 3 and Cg3todaydat5 ne . then todaydat5 = Cg3todaydat5;
		Else If round = 4 and Cg4todaydat4 ne . then todaydat5 = Cg4todaydat4;
		Else If round = 5 and Cg5todaydat5 ne . then todaydat5 = Cg5todaydat5;
		Else If round = 6 and Cg6todaydat5 ne . then todaydat5 = Cg6todaydat5;
		Else If round = 7 and Cg7todaydat5 ne . then todaydat5 = Cg7todaydat5;
		Else If round = 8 and Cg8todaydat5 ne . then todaydat5 = Cg8todaydat5;
		Else todaydat5 = .; 
	/*PRES LAST NAME CORRECT*/
	If round = 1 and Cg1presidna1 ne . then presidna1 = Cg1presidna1;
		Else If round = 2 and Cg2presidna1 ne . then presidna1 = Cg2presidna1;
		Else If round = 3 and Cg3presidna1 ne . then presidna1 = Cg3presidna1;
		Else If round = 4 and Cg4presidna1 ne . then presidna1 = Cg4presidna1;
		Else If round = 5 and Cg5presidna1 ne . then presidna1 = Cg5presidna1;
		Else If round = 6 and Cg6presidna1 ne . then presidna1 = Cg6presidna1;
		Else If round = 7 and Cg7presidna1 ne . then presidna1 = Cg7presidna1;
		Else If round = 8 and Cg8presidna1 ne . then presidna1 = Cg8presidna1;
		Else presidna1 = .; 
	/*PRES FIRST NAME CORRCT*/
	If round = 1 and Cg1presidna3 ne . then presidna3 = Cg1presidna3;
		Else If round = 2 and Cg2presidna3 ne . then presidna3 = Cg2presidna3;
		Else If round = 3 and Cg3presidna3 ne . then presidna3 = Cg3presidna3;
		Else If round = 4 and Cg4presidna3 ne . then presidna3 = Cg4presidna3;
		Else If round = 5 and Cg5presidna3 ne . then presidna3 = Cg5presidna3;
		Else If round = 6 and Cg6presidna3 ne . then presidna3 = Cg6presidna3;
		Else If round = 7 and Cg7presidna3 ne . then presidna3 = Cg7presidna3;
		Else If round = 8 and Cg8presidna3 ne . then presidna3 = Cg8presidna3;
		Else presidna3 = .; 
	/*VP LAST NAME CORRECT*/
	If round = 1 and Cg1vpname1 ne . then vpname1 = Cg1vpname1;
		Else If round = 2 and Cg2vpname1 ne . then vpname1 = Cg2vpname1;
		Else If round = 3 and Cg3vpname1 ne . then vpname1 = Cg3vpname1;
		Else If round = 4 and Cg4vpname1 ne . then vpname1 = Cg4vpname1;
		Else If round = 5 and Cg5vpname1 ne . then vpname1 = Cg5vpname1;
		Else If round = 6 and Cg6vpname1 ne . then vpname1 = Cg6vpname1;
		Else If round = 7 and Cg7vpname1 ne . then vpname1 = Cg7vpname1;
		Else If round = 8 and Cg8vpname1 ne . then vpname1 = Cg8vpname1;
		Else vpname1 = .; 
	/*VP FIRST NAME CORRECT*/
	If round = 1 and Cg1vpname3 ne . then vpname3 = Cg1vpname3;
		Else If round = 2 and Cg2vpname3 ne . then vpname3 = Cg2vpname3;
		Else If round = 3 and Cg3vpname3 ne . then vpname3 = Cg3vpname3;
		Else If round = 4 and Cg4vpname3 ne . then vpname3 = Cg4vpname3;
		Else If round = 5 and Cg5vpname3 ne . then vpname3 = Cg5vpname3;
		Else If round = 6 and Cg6vpname3 ne . then vpname3 = Cg6vpname3;
		Else If round = 7 and Cg7vpname3 ne . then vpname3 = Cg7vpname3;
		Else If round = 8 and Cg8vpname3 ne . then vpname3 = Cg8vpname3;
		Else vpname3 = .; 
	/*ORIENTATION DOMAIN: SUM OF DATE RECALL AND PRESIDENT NAMING*/
	If round = 1 and r1date_prvp ne . then datena_score = r1date_prvp;
		Else If round = 2 and r2date_prvp ne . then datena_score = r2date_prvp;
		Else If round = 3 and r3date_prvp ne . then datena_score = r3date_prvp;
		Else If round = 4 and r4date_prvp ne . then datena_score = r4date_prvp;
		Else If round = 5 and r5date_prvp ne . then datena_score = r5date_prvp;
		Else If round = 6 and r6date_prvp ne . then datena_score = r6date_prvp;
		Else If round = 7 and r7date_prvp ne . then datena_score = r7date_prvp;
		Else If round = 8 and r8date_prvp ne . then datena_score = r8date_prvp;
		Else datena_score = .; 
	/*ORIENTATION DOMAIN: ALGORITHM DECISION*/
	If round = 1 and r1datena65 ne . then datena65 = r1datena65;
		Else If round = 2 and r2datena65 ne . then datena65 = r2datena65;
		Else If round = 3 and r3datena65 ne . then datena65 = r3datena65;
		Else If round = 4 and r4datena65 ne . then datena65 = r4datena65;
		Else If round = 5 and r5datena65 ne . then datena65 = r5datena65;
		Else If round = 6 and r6datena65 ne . then datena65 = r6datena65;
		Else If round = 7 and r7datena65 ne . then datena65 = r7datena65;
		Else If round = 8 and r8datena65 ne . then datena65 = r8datena65;
		Else datena65 = .; 
	/*EXECUTIVE FUNCTION DOMAIN: CLOCK DRAWING SCORE*/
	If round = 1 and r1clock_scorer ne . then clock_scorer = r1clock_scorer;
		Else If round = 2 and r2clock_scorer ne . then clock_scorer = r2clock_scorer;
		Else If round = 3 and r3clock_scorer ne . then clock_scorer = r3clock_scorer;
		Else If round = 4 and r4clock_scorer ne . then clock_scorer = r4clock_scorer;
		Else If round = 5 and r5clock_scorer ne . then clock_scorer = r5clock_scorer;
		Else If round = 6 and r6clock_scorer ne . then clock_scorer = r6clock_scorer;
		Else If round = 7 and r7clock_scorer ne . then clock_scorer = r7clock_scorer;
		Else If round = 8 and r8clock_scorer ne . then clock_scorer = r8clock_scorer;
		Else clock_scorer = .;
	/*EXECUTIVE FUNCTION DOMAIN: ALGORITHM DECISION*/
	If round = 1 and r1clock65 ne . then clock65 = r1clock65;
		Else If round = 2 and r2clock65 ne . then clock65 = r2clock65;
		Else If round = 3 and r3clock65 ne . then clock65 = r3clock65;
		Else If round = 4 and r4clock65 ne . then clock65 = r4clock65;
		Else If round = 5 and r5clock65 ne . then clock65 = r5clock65;
		Else If round = 6 and r6clock65 ne . then clock65 = r6clock65;
		Else If round = 7 and r7clock65 ne . then clock65 = r7clock65;
		Else If round = 8 and r8clock65 ne . then clock65 = r8clock65;
		Else clock65 = .;
	/*SCORE IMMEDIATE WORD RECALL*/	
	If round = 1 and cg1dwrdimmrc ne . then wrdimmrc = cg1dwrdimmrc;
		Else If round = 2 and cg2dwrdimmrc ne . then wrdimmrc = cg2dwrdimmrc;
		Else If round = 3 and cg3dwrdimmrc ne . then wrdimmrc = cg3dwrdimmrc;
		Else If round = 4 and cg4dwrdimmrc ne . then wrdimmrc = cg4dwrdimmrc;
		Else If round = 5 and cg5dwrdimmrc ne . then wrdimmrc = cg5dwrdimmrc;
		Else If round = 6 and cg6dwrdimmrc ne . then wrdimmrc = cg6dwrdimmrc;
		Else If round = 7 and cg7dwrdimmrc ne . then wrdimmrc = cg7dwrdimmrc;
		Else If round = 8 and cg8dwrdimmrc ne . then wrdimmrc = cg8dwrdimmrc;
		Else wrdimmrc = .;
	/*SCORE DELAYED WORD RECALL*/
	If round = 1 and cg1dwrddlyrc ne . then wrddlyrc = cg1dwrddlyrc;
		Else If round = 2 and cg2dwrddlyrc ne . then wrddlyrc = cg2dwrddlyrc;
		Else If round = 3 and cg3dwrddlyrc ne . then wrddlyrc = cg3dwrddlyrc;
		Else If round = 4 and cg4dwrddlyrc ne . then wrddlyrc = cg4dwrddlyrc;
		Else If round = 5 and cg5dwrddlyrc ne . then wrddlyrc = cg5dwrddlyrc;
		Else If round = 6 and cg6dwrddlyrc ne . then wrddlyrc = cg6dwrddlyrc;
		Else If round = 7 and cg7dwrddlyrc ne . then wrddlyrc = cg7dwrddlyrc;
		Else If round = 8 and cg8dwrddlyrc ne . then wrddlyrc = cg8dwrddlyrc;
		Else wrddlyrc = .;
	/*MEMORY DOMAIN: IMMEDIATE AND DELAYED WORD RECALL SCORE*/
	If round = 1 and r1wordrecall0_20 ne . then wordrecall0_20 = r1wordrecall0_20;
		Else If round = 2 and r2wordrecall0_20 ne . then wordrecall0_20 = r2wordrecall0_20;
		Else If round = 3 and r3wordrecall0_20 ne . then wordrecall0_20 = r3wordrecall0_20;
		Else If round = 4 and r4wordrecall0_20 ne . then wordrecall0_20 = r4wordrecall0_20;
		Else If round = 5 and r5wordrecall0_20 ne . then wordrecall0_20 = r5wordrecall0_20;
		Else If round = 6 and r6wordrecall0_20 ne . then wordrecall0_20 = r6wordrecall0_20;
		Else If round = 7 and r7wordrecall0_20 ne . then wordrecall0_20 = r7wordrecall0_20;
		Else If round = 8 and r8wordrecall0_20 ne . then wordrecall0_20 = r8wordrecall0_20;
		Else wordrecall0_20 = .;
	/*MEMORY DOMAIN: ALGORITHM DECISION*/
	If round = 1 and r1word65 ne . then word65 = r1word65;
		Else If round = 2 and r2word65 ne . then word65 = r2word65;
		Else If round = 3 and r3word65 ne . then word65 = r3word65;
		Else If round = 4 and r4word65 ne . then word65 = r4word65;
		Else If round = 5 and r5word65 ne . then word65 = r5word65;
		Else If round = 6 and r6word65 ne . then word65 = r6word65;
		Else If round = 7 and r7word65 ne . then word65 = r7word65;
		Else If round = 8 and r8word65 ne . then word65 = r8word65;
		Else word65 = .;
	/*NHATS Dementia Classification 65+*/
	If round = 1 and R1demclas ne . then demclas = R1demclas;
		Else If round = 2 and R2demclas ne . then demclas = R2demclas;
		Else If round = 3 and R3demclas ne . then demclas = R3demclas;
		Else If round = 4 and R4demclas ne . then demclas = R4demclas;
		Else If round = 5 and R5demclas ne . then demclas = R5demclas;
		Else If round = 6 and R6demclas ne . then demclas = R6demclas;
		Else If round = 7 and R7demclas ne . then demclas = R7demclas;
		Else If round = 8 and R8demclas ne . then demclas = R8demclas;
		Else demclas = .;
	/*SP LITTLE INTERST PLEAS*/
	If round = 1 and hc1depresan1 ne . then depresan1 = hc1depresan1;
		Else If round = 2 and hc2depresan1 ne . then depresan1 = hc2depresan1;
		Else If round = 3 and hc3depresan1 ne . then depresan1 = hc3depresan1;
		Else If round = 4 and hc4depresan1 ne . then depresan1 = hc4depresan1;
		Else If round = 5 and hc5depresan1 ne . then depresan1 = hc5depresan1;
		Else If round = 6 and hc6depresan1 ne . then depresan1 = hc6depresan1;
		Else If round = 7 and hc7depresan1 ne . then depresan1 = hc7depresan1;
		Else If round = 8 and hc8depresan1 ne . then depresan1 = hc8depresan1;
		Else depresan1 = .;
	/*SP DOWN DEPRES HOPELESS*/
	If round = 1 and hc1depresan2 ne . then depresan2 = hc1depresan2;
		Else If round = 2 and hc2depresan2 ne . then depresan2 = hc2depresan2;
		Else If round = 3 and hc3depresan2 ne . then depresan2 = hc3depresan2;
		Else If round = 4 and hc4depresan2 ne . then depresan2 = hc4depresan2;
		Else If round = 5 and hc5depresan2 ne . then depresan2 = hc5depresan2;
		Else If round = 6 and hc6depresan2 ne . then depresan2 = hc6depresan2;
		Else If round = 7 and hc7depresan2 ne . then depresan2 = hc7depresan2;
		Else If round = 8 and hc8depresan2 ne . then depresan2 = hc8depresan2;
		Else depresan2 = .;
	/*SP NERVOUS ANXIOUS*/
	If round = 1 and hc1depresan3 ne . then depresan3 = hc1depresan3;
		Else If round = 2 and hc2depresan3 ne . then depresan3 = hc2depresan3;
		Else If round = 3 and hc3depresan3 ne . then depresan3 = hc3depresan3;
		Else If round = 4 and hc4depresan3 ne . then depresan3 = hc4depresan3;
		Else If round = 5 and hc5depresan3 ne . then depresan3 = hc5depresan3;
		Else If round = 6 and hc6depresan3 ne . then depresan3 = hc6depresan3;
		Else If round = 7 and hc7depresan3 ne . then depresan3 = hc7depresan3;
		Else If round = 8 and hc8depresan3 ne . then depresan3 = hc8depresan3;
		Else depresan3 = .;
	/*SP UNABLE TO STOP WORRY*/
	If round = 1 and hc1depresan4 ne . then depresan4 = hc1depresan4;
		Else If round = 2 and hc2depresan4 ne . then depresan4 = hc2depresan4;
		Else If round = 3 and hc3depresan4 ne . then depresan4 = hc3depresan4;
		Else If round = 4 and hc4depresan4 ne . then depresan4 = hc4depresan4;
		Else If round = 5 and hc5depresan4 ne . then depresan4 = hc5depresan4;
		Else If round = 6 and hc6depresan4 ne . then depresan4 = hc6depresan4;
		Else If round = 7 and hc7depresan4 ne . then depresan4 = hc7depresan4;
		Else If round = 8 and hc8depresan4 ne . then depresan4 = hc8depresan4;
		Else depresan4 = .;
	/*OVERALL HEALTH CONDITION*/
	If round = 1 and Hc1health ne . then health = Hc1health;
		Else If round = 2 and Hc2health ne . then health = Hc2health;
		Else If round = 3 and Hc3health ne . then health = Hc3health;
		Else If round = 4 and Hc4health ne . then health = Hc4health;
		Else If round = 5 and Hc5health ne . then health = Hc5health;
		Else If round = 6 and Hc6health ne . then health = Hc6health;
		Else If round = 7 and Hc7health ne . then health = Hc7health;
		Else If round = 8 and Hc8health ne . then health = Hc8health;
		Else health = .;
	/*BOTHERED BY PAIN*/
	If round = 1 and ss1painbothr ne . then painbothr = ss1painbothr;
		Else If round = 2 and ss2painbothr ne . then painbothr = ss2painbothr;
		Else If round = 3 and ss3painbothr ne . then painbothr = ss3painbothr;
		Else If round = 4 and ss4painbothr ne . then painbothr = ss4painbothr;
		Else If round = 5 and ss5painbothr ne . then painbothr = ss5painbothr;
		Else If round = 6 and ss6painbothr ne . then painbothr = ss6painbothr;
		Else If round = 7 and ss7painbothr ne . then painbothr = ss7painbothr;
		Else If round = 8 and ss8painbothr ne . then painbothr = ss8painbothr;
		Else painbothr = .;
	/*HAS HELP EATING*/
	If round = 1 and sc1deathelp ne . then eathelp = sc1deathelp;
		Else If round = 2 and sc2deathelp ne . then eathelp = sc2deathelp;
		Else If round = 3 and sc3deathelp ne . then eathelp = sc3deathelp;
		Else If round = 4 and sc4deathelp ne . then eathelp = sc4deathelp;
		Else If round = 5 and sc5deathelp ne . then eathelp = sc5deathelp;
		Else If round = 6 and sc6deathelp ne . then eathelp = sc6deathelp;
		Else If round = 7 and sc7deathelp ne . then eathelp = sc7deathelp;
		Else If round = 8 and sc8deathelp ne . then eathelp = sc8deathelp;
		Else eathelp = .;
	/*HAS HELP WHILE BATHING*/
	If round = 1 and sc1dbathhelp ne . then bathhelp = sc1dbathhelp;
		Else If round = 2 and sc2dbathhelp ne . then bathhelp = sc2dbathhelp;
		Else If round = 3 and sc3dbathhelp ne . then bathhelp = sc3dbathhelp;
		Else If round = 4 and sc4dbathhelp ne . then bathhelp = sc4dbathhelp;
		Else If round = 5 and sc5dbathhelp ne . then bathhelp = sc5dbathhelp;
		Else If round = 6 and sc6dbathhelp ne . then bathhelp = sc6dbathhelp;
		Else If round = 7 and sc7dbathhelp ne . then bathhelp = sc7dbathhelp;
		Else If round = 8 and sc8dbathhelp ne . then bathhelp = sc8dbathhelp;
		Else bathhelp = .;
	/*HAS HELP WHILE TOILETING*/
	If round = 1 and sc1dtoilhelp ne . then toilhelp = sc1dtoilhelp;
		Else If round = 2 and sc2dtoilhelp ne . then toilhelp = sc2dtoilhelp;
		Else If round = 3 and sc3dtoilhelp ne . then toilhelp = sc3dtoilhelp;
		Else If round = 4 and sc4dtoilhelp ne . then toilhelp = sc4dtoilhelp;
		Else If round = 5 and sc5dtoilhelp ne . then toilhelp = sc5dtoilhelp;
		Else If round = 6 and sc6dtoilhelp ne . then toilhelp = sc6dtoilhelp;
		Else If round = 7 and sc7dtoilhelp ne . then toilhelp = sc7dtoilhelp;
		Else If round = 8 and sc8dtoilhelp ne . then toilhelp = sc8dtoilhelp;
		Else toilhelp = .;
	/*HAS HELP WHILE DRESSING*/
	If round = 1 and Sc1ddreshelp ne . then dreshelp = Sc1ddreshelp;
		Else If round = 2 and Sc2ddreshelp ne . then dreshelp = Sc2ddreshelp;
		Else If round = 3 and Sc3ddreshelp ne . then dreshelp = Sc3ddreshelp;
		Else If round = 4 and Sc4ddreshelp ne . then dreshelp = Sc4ddreshelp;
		Else If round = 5 and Sc5ddreshelp ne . then dreshelp = Sc5ddreshelp;
		Else If round = 6 and Sc6ddreshelp ne . then dreshelp = Sc6ddreshelp;
		Else If round = 7 and Sc7ddreshelp ne . then dreshelp = Sc7ddreshelp;
		Else If round = 8 and Sc8ddreshelp ne . then dreshelp = Sc8ddreshelp;
		Else dreshelp = .;
	/*MOVE INSIDE WITH HELP*/
	If round = 1 and mo1dinsdhelp ne . then insdhelp = mo1dinsdhelp;
		Else If round = 2 and mo2dinsdhelp ne . then insdhelp = mo2dinsdhelp;
		Else If round = 3 and mo3dinsdhelp ne . then insdhelp = mo3dinsdhelp;
		Else If round = 4 and mo4dinsdhelp ne . then insdhelp = mo4dinsdhelp;
		Else If round = 5 and mo5dinsdhelp ne . then insdhelp = mo5dinsdhelp;
		Else If round = 6 and mo6dinsdhelp ne . then insdhelp = mo6dinsdhelp;
		Else If round = 7 and mo7dinsdhelp ne . then insdhelp = mo7dinsdhelp;
		Else If round = 8 and mo8dinsdhelp ne . then insdhelp = mo8dinsdhelp;
		Else insdhelp = .;
	/*HELP TO GET OUT OF BED*/
	If round = 1 and mo1dbedhelp ne . then bedhelp = mo1dbedhelp;
		Else If round = 2 and mo2dbedhelp ne . then bedhelp = mo2dbedhelp;
		Else If round = 3 and mo3dbedhelp ne . then bedhelp = mo3dbedhelp;
		Else If round = 4 and mo4dbedhelp ne . then bedhelp = mo4dbedhelp;
		Else If round = 5 and mo5dbedhelp ne . then bedhelp = mo5dbedhelp;
		Else If round = 6 and mo6dbedhelp ne . then bedhelp = mo6dbedhelp;
		Else If round = 7 and mo7dbedhelp ne . then bedhelp = mo7dbedhelp;
		Else If round = 8 and mo8dbedhelp ne . then bedhelp = mo8dbedhelp;
		Else bedhelp = .;
	/*GO OUTSIDE USING HELP*/
	If round = 1 and mo1douthelp ne . then outhelp = mo1douthelp;
		Else If round = 2 and mo2douthelp ne . then outhelp = mo2douthelp;
		Else If round = 3 and mo3douthelp ne . then outhelp = mo3douthelp;
		Else If round = 4 and mo4douthelp ne . then outhelp = mo4douthelp;
		Else If round = 5 and mo5douthelp ne . then outhelp = mo5douthelp;
		Else If round = 6 and mo6douthelp ne . then outhelp = mo6douthelp;
		Else If round = 7 and mo7douthelp ne . then outhelp = mo7douthelp;
		Else If round = 8 and mo8douthelp ne . then outhelp = mo8douthelp;
		Else outhelp = .;
	/*OFTEN YOU FEEL CHEERFUL*/
	If round = 1 and Wb1offelche1 ne . then offelche1 = Wb1offelche1;
		Else If round = 2 and Wb2offelche1 ne . then offelche1 = Wb2offelche1;
		Else If round = 3 and Wb3offelche1 ne . then offelche1 = Wb3offelche1;
		Else If round = 4 and Wb4offelche1 ne . then offelche1 = Wb4offelche1;
		Else If round = 5 and Wb5offelche1 ne . then offelche1 = Wb5offelche1;
		Else If round = 6 and Wb6offelche1 ne . then offelche1 = Wb6offelche1;
		Else If round = 7 and Wb7offelche1 ne . then offelche1 = Wb7offelche1;
		Else If round = 8 and Wb8offelche1 ne . then offelche1 = Wb8offelche1;
		Else offelche1 = .;
	/*OFTEN YOU FEEL BORED*/
	If round = 1 and Wb1offelche2 ne . then offelche2 = Wb1offelche2;
		Else If round = 2 and Wb2offelche2 ne . then offelche2 = Wb2offelche2;
		Else If round = 3 and Wb3offelche2 ne . then offelche2 = Wb3offelche2;
		Else If round = 4 and Wb4offelche2 ne . then offelche2 = Wb4offelche2;
		Else If round = 5 and Wb5offelche2 ne . then offelche2 = Wb5offelche2;
		Else If round = 6 and Wb6offelche2 ne . then offelche2 = Wb6offelche2;
		Else If round = 7 and Wb7offelche2 ne . then offelche2 = Wb7offelche2;
		Else If round = 8 and Wb8offelche2 ne . then offelche2 = Wb8offelche2;
		Else offelche2 = .;
	/*OFTEN YOU FEEL FULL OF LIFE*/
	If round = 1 and Wb1offelche3 ne . then offelche3 = Wb1offelche3;
		Else If round = 2 and Wb2offelche3 ne . then offelche3 = Wb2offelche3;
		Else If round = 3 and Wb3offelche3 ne . then offelche3 = Wb3offelche3;
		Else If round = 4 and Wb4offelche3 ne . then offelche3 = Wb4offelche3;
		Else If round = 5 and Wb5offelche3 ne . then offelche3 = Wb5offelche3;
		Else If round = 6 and Wb6offelche3 ne . then offelche3 = Wb6offelche3;
		Else If round = 7 and Wb7offelche3 ne . then offelche3 = Wb7offelche3;
		Else If round = 8 and Wb8offelche3 ne . then offelche3 = Wb8offelche3;
		Else offelche3 = .;
	/*OFTEN YOU FEEL UPSET*/
	If round = 1 and Wb1offelche4 ne . then offelche4 = Wb1offelche4;
		Else If round = 2 and Wb2offelche4 ne . then offelche4 = Wb2offelche4;
		Else If round = 3 and Wb3offelche4 ne . then offelche4 = Wb3offelche4;
		Else If round = 4 and Wb4offelche4 ne . then offelche4 = Wb4offelche4;
		Else If round = 5 and Wb5offelche4 ne . then offelche4 = Wb5offelche4;
		Else If round = 6 and Wb6offelche4 ne . then offelche4 = Wb6offelche4;
		Else If round = 7 and Wb7offelche4 ne . then offelche4 = Wb7offelche4;
		Else If round = 8 and Wb8offelche4 ne . then offelche4 = Wb8offelche4;
		Else offelche4 = .;
	/*LIFE HAS MEANING PURPS*/
	If round = 1 and Wb1truestme1 ne . then truestme1 = Wb1truestme1;
		Else If round = 2 and Wb2truestme1 ne . then truestme1 = Wb2truestme1;
		Else If round = 3 and Wb3truestme1 ne . then truestme1 = Wb3truestme1;
		Else If round = 4 and Wb4truestme1 ne . then truestme1 = Wb4truestme1;
		Else If round = 5 and Wb5truestme1 ne . then truestme1 = Wb5truestme1;
		Else If round = 6 and Wb6truestme1 ne . then truestme1 = Wb6truestme1;
		Else If round = 7 and Wb7truestme1 ne . then truestme1 = Wb7truestme1;
		Else If round = 8 and Wb8truestme1 ne . then truestme1 = Wb8truestme1;
		Else truestme1 = .;
	/*FEELS CONFIDENT*/
	If round = 1 and Wb1truestme2 ne . then truestme2 = Wb1truestme2;
		Else If round = 2 and Wb2truestme2 ne . then truestme2 = Wb2truestme2;
		Else If round = 3 and Wb3truestme2 ne . then truestme2 = Wb3truestme2;
		Else If round = 4 and Wb4truestme2 ne . then truestme2 = Wb4truestme2;
		Else If round = 5 and Wb5truestme2 ne . then truestme2 = Wb5truestme2;
		Else If round = 6 and Wb6truestme2 ne . then truestme2 = Wb6truestme2;
		Else If round = 7 and Wb7truestme2 ne . then truestme2 = Wb7truestme2;
		Else If round = 8 and Wb8truestme2 ne . then truestme2 = Wb8truestme2;
		Else truestme2 = .;
	/*GAVE UP IMPROVING LIFE*/
	If round = 1 and Wb1truestme3 ne . then truestme3 = Wb1truestme3;
		Else If round = 2 and Wb2truestme3 ne . then truestme3 = Wb2truestme3;
		Else If round = 3 and Wb3truestme3 ne . then truestme3 = Wb3truestme3;
		Else If round = 4 and Wb4truestme3 ne . then truestme3 = Wb4truestme3;
		Else If round = 5 and Wb5truestme3 ne . then truestme3 = Wb5truestme3;
		Else If round = 6 and Wb6truestme3 ne . then truestme3 = Wb6truestme3;
		Else If round = 7 and Wb7truestme3 ne . then truestme3 = Wb7truestme3;
		Else If round = 8 and Wb8truestme3 ne . then truestme3 = Wb8truestme3;
		Else truestme3 = .;
	/*LIKES LIVING SITUATION*/
	If round = 1 and Wb1truestme4 ne . then truestme4 = Wb1truestme4;
		Else If round = 2 and Wb2truestme4 ne . then truestme4 = Wb2truestme4;
		Else If round = 3 and Wb3truestme4 ne . then truestme4 = Wb3truestme4;
		Else If round = 4 and Wb4truestme4 ne . then truestme4 = Wb4truestme4;
		Else If round = 5 and Wb5truestme4 ne . then truestme4 = Wb5truestme4;
		Else If round = 6 and Wb6truestme4 ne . then truestme4 = Wb6truestme4;
		Else If round = 7 and Wb7truestme4 ne . then truestme4 = Wb7truestme4;
		Else If round = 8 and Wb8truestme4 ne . then truestme4 = Wb8truestme4;
		Else truestme4 = .;
Run;

Proc sort data=NHATS_pooled4; By spid round; Run;

/*Create baseline and flag variables*/
Data NHATS_pooled5; Set NHATS_pooled4;
	By spid;

	/*Proxy report Dem/Alz in AD8 questions resulting in AD8 score  = 8*/
	If round = 1 and chgthink1 = 3 and ad8_score = 8 then ad8_flag = 1;
		Else if round = 1 and chgthink2 = 3 and ad8_score = 8 then ad8_flag = 1;
		Else if round = 1 and chgthink3 = 3 and ad8_score = 8 then ad8_flag = 1;
		Else if round = 1 and chgthink4 = 3 and ad8_score = 8 then ad8_flag = 1;
		Else if round = 1 and chgthink5 = 3 and ad8_score = 8 then ad8_flag = 1;
		Else if round = 1 and chgthink6 = 3 and ad8_score = 8 then ad8_flag = 1;
		Else if round = 1 and chgthink7 = 3 and ad8_score = 8 then ad8_flag = 1;
		Else if round = 1 and chgthink8 = 3 and ad8_score = 8 then ad8_flag = 1;
		Else if ad8_proxydem = 1 and ad8_score = 8 then ad8_flag = 1;
		Else ad8_flag=0; 

	/*gender*/
	If first_obs = 1 and r1dgender ne . then bl_gender = r1dgender;
		Else if first_obs = 1 and r5dgender ne . then bl_gender = r5dgender;
	/*racehisp*/
	If first_obs = 1 and rl1dracehisp ne . then bl_racehisp = rl1dracehisp;
		Else if first_obs = 1 and rl5dracehisp ne . then bl_racehisp = rl5dracehisp;
	/*higstschl*/
	If first_obs = 1 and el1higstschl ne . then bl_higstschl = el1higstschl;
		Else if first_obs = 1 and el5higstschl ne . then bl_higstschl = el5higstschl;
	/*borninus*/
	If first_obs = 1 and el1borninus ne . then bl_borninus = el1borninus;
		Else if first_obs = 1 and el5borninus ne . then bl_borninus = el5borninus;
	/*age2us*/
	If first_obs = 1 and el1dage2us ne . then bl_age2us = el1dage2us;
		Else if first_obs = 1 and el5dage2us ne . then bl_age2us = el5dage2us;
	/*baseline age cat*/
	If first_obs = 1 and r1d2intvrage ne . then bl_agecat = r1d2intvrage;
		Else if first_obs = 1 and r2d2intvrage ne . then bl_agecat = r2d2intvrage;
		Else if first_obs = 1 and r3d2intvrage ne . then bl_agecat = r3d2intvrage;
		Else if first_obs = 1 and r4d2intvrage ne . then bl_agecat = r4d2intvrage;
		Else if first_obs = 1 and r5d2intvrage ne . then bl_agecat = r5d2intvrage;
		Else if first_obs = 1 and r6d2intvrage ne . then bl_agecat = r6d2intvrage;
		Else if first_obs = 1 and r7d2intvrage ne . then bl_agecat = r7d2intvrage;
		Else if first_obs = 1 and r8d2intvrage ne . then bl_agecat = r8d2intvrage;

	Retain bl_gender bl_racehisp bl_higstschl bl_borninus bl_age2us bl_agecat;
	Run;


/******************************************
	Create formats for derived vars
******************************************/
Proc format library=NHATS;
	value gender
		1 = 'MALE'
		2 = 'FEMALE'
	;
	value racehisp
		1 = 'White, non-hispanic'
		2 = 'Black, non-hispanic'
		3 = 'Other (Am Indian/Asian/Native Hawaiian/Pacific Islander/other specify), non-Hispanic'
		4 = 'Hispanic'
		5 = 'more than one DKRF primary'
		6 = 'DKRF'
	;
	value higstschl
		-7 = 'Refusal'
   		-8 = 'Dont Know'
		1  = 'NO SCHOOL COMPLETED'
		2  = '1ST-8TH GRADE'
		3  = '9TH-12TH GRADE (NO DIPLOMA)'
		4  = 'HIGH SCHOOL GRADUATE (HIGH SCHOOL DIPLOMA OR EQUIVALENT)'
		5  = 'VOCATIONAL, TECHNICAL, BUSINESS, OR TRADE SCHOOL CERTIFICATE OR DIPLOMA (BEYOND HIGH SCHOOL LEVEL)'
		6  = 'SOME COLLEGE BUT NO DEGREE'
		7  = 'ASSOCIATES DEGREE'
		8  = 'BACHELORS DEGREE'
		9  = 'MASTERS, PROFESSIONAL, OR DOCTORAL DEGREE'
	;
	value borninus
		-7 = 'Refusal'
   		-8 = 'Dont Know'
		1  = 'YES'
		2  = 'NO'
	;
	value agecat
		1 = "65 to 69"
		2 = "70 to 74"
		3 = "75 to 79"
		4 = "80 to 84"
		5 = "85 to 89"
		6 = "90+"
	;
	value resid
		1 = 'Community'
		2 = 'Residential care not nursing home (SP interview)'
	;
	value censdiv
		1 = 'Northeast Region: New England Division (ME, NH, VT, MA, RI, CT)'
		2 = 'Northeast Region: Middle Atlantic Division (NY, NJ, PA)'
		3 = 'Midwest Region: East North Central Division (OH, IN, IL, MI, WI)'
		4 = 'Midwest Region: West North Central Division (MN, IA, MO, ND, SD, NE, KS)'
		5 = 'South Region: South Atlantic Division (DE, MD, DC, VA, WV, NC, SC, GA, FL)'
		6 = 'South Region: East South Central Division (KY, TN, AL, MS)'
		7 = 'South Region: West South Central Division (AR, LA, OK, TX)'
		8 = 'West Region: Mountain Division (MT, ID, WY, CO, NM, AZ, UT, NV)'
		9 = 'West Region: Pacific Division (WA, OR, CA, AK, HI)'
	;
	value metro
		1 = 'Metro'
		2 = 'Nonmetro'
	;
	value resptype
		1 = 'SAMPLE PERSON (SP)'
		2 = 'PROXY'
	;
	value relat
		-9 = 'Missing'
		-1 = 'Inapplicable'
		2  = 'SPOUSE/ PARTNER'
		3  = 'DAUGHTER'
		4  = 'SON'
		5  = 'DAUGHTER-IN-LAW'
		6  = 'SON-IN-LAW'
		7  = 'STEPDAUGHTER'
		8  = 'STEPSON'
		9  = 'SISTER'
		10 = 'BROTHER'
		11 = 'SISTER-IN-LAW'
		12 = 'BROTHER-IN-LAW'
		13 = 'MOTHER'
		16 = 'FATHER'
		19 = 'GRANDDAUGHTER'
		20 = 'GRANDSON'
		21 = 'NIECE'
		22 = 'NEPHEW'
		23 = 'AUNT'
		25 = 'COUSIN'
		26 = 'STEPDAUGHTERS SON/ DAUGHTER'
		27 = 'STEPSONS SON/ DAUGHTER'
		28 = 'DAUGHTER-IN-LAWS SON/ DAUGHTER'
		29 = 'SON-IN-LAWS SON/ DAUGHTER'
		31 = 'PAID AIDE/ HOUSEKEEPER/ EMPLOYEE'
		32 = 'ROOMMATE'
		33 = 'EX-WIFE/ EX-HUSBAND'
		34 = 'BOYFRIEND/ GIRLFRIEND'
		35 = 'NEIGHBOR'
		36 = 'FRIEND'
		37 = 'SOMEONE/SERVICE FROM THE PLACE SP LIVES/DIED'
		39 = 'MINISTER, PRIEST, OR OTHER CLERGY'
		40 = 'PSYCHIATRIST, PSYCHOLOGIST, COUNSELOR, OR THERAPIST'
		91 = 'OTHER RELATIVE'
		92 = 'OTHER NONRELATIVE'
	;
	value familiar
		-1 = 'Inapplicable'
		1  = 'VERY FAMILIAR'
		2  = 'SOMEWHAT FAMILIAR'
		3  = 'A LITTLE FAMILIAR'
		4  = 'NOT AT ALL FAMILIAR'
	;
	value livsp
		-1 = 'Inapplicable'
		1  = 'YES'
		2  = 'NO'
	;
	value lvngarrg
		1 = 'Alone'
		2 = 'With spouse/partner only [spouse/partner in household]'
		3 = 'With spouse/partner and with others'
		4 = 'With others only'
	;
	value dises
		1 = 'YES'
		2 = 'NO'
		7 = 'PREVIOUSLY REPORTED'
	;
	value ratemem
		1 = 'EXCELLENT'
		2 = 'VERY GOOD'
		3 = 'GOOD'
		4 = 'FAIR'
		5 = 'POOR'	
	;
	value chgthink
		-1 = 'Inapplicable'
		1 = 'YES, A CHANGE'
		2 = 'NO, NO CHANGE'
		3 = 'DEMENTIA/ALZHEIMERS REPORTED BY PROXY'
	;
	
	value demclas
		1  = '1:Probable dementia'
		2  = '2:Possible dementia'
		3  = '3:No dementia'
		-1 = 'Deceased or nursing home resident'
		-9 = 'Missing'
	;
	value depres
		-7 = 'Refusal'
   		-8 = 'Dont Know'
		1  = 'NOT AT ALL'
		2  = 'SEVERAL DAYS'
		3  = 'MORE THAN HALF THE DAYS'
		4  = 'NEARLY EVERY DAY'
	;
	value health
		-7 = 'Refusal'
   		-8 = 'Dont Know'
		1  = 'EXCELLENT'
		2  = 'VERY GOOD'
		3  = 'GOOD'
		4  = 'FAIR'
		5  = 'POOR'
	;
	value pain
		-7 = 'Refusal'
		-8 = 'Dont know'
		1  = 'YES'
		2  = 'NO'
	;
	value eathelp
		-1 = 'Inapplicable'
		1  = 'No help eating in last month'
		2  = 'Had help eating in last month'
		3  = 'DKRF If had help eating in last month'
		8  = 'Not done in last month'
	;
	value bathhelp 	
		-1 = 'Inapplicable'
		1  = 'No help bathing in last month'
		2  = 'Had help bathing in last month'
		3  = 'DKRF if had help bathing in last month'
	;
	value toilhelp
		-1 = 'Inapplicable'
		1  = 'No help toileting in last month'
		2  = 'Had help toileting in last month'
		3  = 'DKRF If had help toileting in last month'
	;
	value dreshelp
		-1 = 'Inapplicable'
		1  = 'No help dressing in last month'
		2  = 'Had help dressing in last month'
		3  = 'DKRF if had help dressing in last month'
		8  = 'Not done in last month'
		9  = 'DKRF if dressed in last month'
	;
	value insdhelp	
		-1 = 'Inapplicable'
		1  = 'No help to go around inside in last month'
		2  = 'Had help to go around inside in last month'
		3  = 'DKRF If had help to go around inside in last month'
		8  = 'Not done in last month'
	;
	value bedhelp	
		-1 = 'Inapplicable'
		1  = 'No help to get out of bed in last month'
		2  = 'Had help to get out of bed in last month'
		3  = 'DKRF if had help to get out of bed in last month'
	;
	value outhelp	
		-1 = 'Inapplicable'
		1  = 'No help to go outside in last month'
		2  = 'Had help to go outside in last month'
		3  = 'DKRF If had help to go outside in last month'
		8  = 'Not done in last month'
	;
	value offelche
		-9 = 'Missing'
		-8 = 'DK'
		-7 = 'RF'
		-1 = 'Inapplicable'
		1  = 'EVERY DAY (7 DAYS A WEEK)'
		2  = 'MOST DAYS (5-6 DAYS A WEEK)'
		3  = 'SOME DAYS (2-4 DAYS A WEEK)'
		4  = 'RARELY (ONCE A WEEK OR LESS)'
		5  = 'NEVER'
	;
	value truestme
		-9 = 'Missing'
		-8 = 'DK'
		-7 = 'RF'
		-1 = 'Inapplicable'
		1  = 'AGREE A LOT'
		2  = 'AGREE A LITTLE'
		3  = 'AGREE NOT AT ALL'
	;
Run;

Data NHATS_clean; Set NHATS_pooled5; 
format intvrage bl_agecat agecat. bl_racehisp racehisp. bl_higstschl higstschl. bl_borninus borninus. resid resid. censdiv censdiv. metnonmet metro. resptype resptype. prxyrelat relat. famrrutin familiar.
		proxlivsp livsp. lvngarrg lvngarrg. disescn3 disescn6 disescn8 disescn9 disescn10 dises. ratememry ratemem.
		chgthink1 chgthink2 chgthink3 chgthink4 chgthink5 chgthink6 chgthink7 chgthink8 chgthink. demclas demclas.
		depresan1 depresan2 depresan3 depresan4 depres. health health. painbothr pain.
		eathelp eathelp. bathhelp bathhelp. toilhelp toilhelp. dreshelp dreshelp. insdhelp insdhelp. bedhelp bedhelp. outhelp outhelp.
		offelche1 offelche2 offelche3 offelche4 offelche. truestme1 truestme2 truestme3 truestme4 truestme. ;
Run;


/*******************
	Save dataset
*******************/

Data qol.NHATS_QOLDEM_clean; Set NHATS_clean; Run;
/*The data set QOL.NHATS_QOLDEM_CLEAN has 46444 observations and 681 variables*/
