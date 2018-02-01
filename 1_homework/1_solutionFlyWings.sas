DATA flyWings;
input Continent $      Latitude      Females      SE_Females      Males      SE_Males      Ratio      SE_Ratio;
DATAlines;
NA      35.5      901      2.5      797      3.8      0.831      0.01
NA      37      896      3.5      806      3      0.834      0.014
NA      38.6      906      3      812      3.2      0.836      0.012
NA      40.7      907      3.5      807      3.2      0.833      0.013
NA      40.9      898      3.6      818      2.7      0.83      0.012
NA      42.4      893      3.4      809      3.3      0.828      0.015
NA      45      913      4.3      810      4.3      0.834      0.024
NA      46.8      915      3.8      819      3.3      0.825      0.014
NA      48.8      927      2      800      4.9      0.832      0.009
NA      49.8      924      4.5      823      2.2      0.824      0.011
NA      50.8      930      3.4      814      4.1      0.826      0.013
EU      36.4      905      6.1      789      5.9      0.822      0.016
EU      39.3      889      4.7      803      3.5      0.809      0.021
EU      41.3      915      2.1      812      3.7      0.821      0.007
EU      43.4      930      2.6      820      3.1      0.831      0.016
EU      45.5      895      4.4      808      2.9      0.821      0.017
EU      47.3      926      3.2      815      6.2      0.826      0.01
EU      48.5      944      4.8      855      3.1      0.825      0.032
EU      50.4      925      3.2      842      4.3      0.82      0.013
EU      52.1      920      3.9      819      3.1      0.834      0.019
EU      56.1      934      4.2      839      4.4      0.825      0.012
;

DATA females;
	SET flyWings;
	gender = 'F';
	wing = females;
	KEEP continent;
	KEEP latitude;
	KEEP wing;
	KEEP gender;
;


DATA males;
	SET flyWings;
	gender = 'M';
	wing = males;
	KEEP Continent;
	KEEP Latitude;
	KEEP wing;
	KEEP gender;
;

DATA flyWings;
	set females males;
	if Continent eq 'NA' and gender = 'F' then cross = 'FNA';
	if Continent eq 'NA' and gender = 'M' then cross = 'MNA';
	if Continent eq 'EU' and gender = 'F' then cross = 'FEU';
	if Continent eq 'EU' and gender = 'M' then cross = 'MEU';
RUN;


PROC PRINT DATA = flyWings;
RUN;

PROC SGSCATTER DATA = flyWings;
	TITLE "WingSize versus Latitude by Continent and Gender";
	MATRIX latitude wing / group = cross diagonal = (histogram);
RUN;

PROC GLM DATA = flyWings PLOTS = ALL;
	CLASS continent (ref = "EU") gender (ref = "F");
	MODEL wing = latitude | gender | continent / SOLUTION CLPARM;	
RUN;

PROC GLM DATA = flyWings PLOTS = ALL;
	CLASS continent (ref = "EU") gender (ref = "F");
	MODEL wing = latitude gender continent / SOLUTION CLPARM;	
RUN;

