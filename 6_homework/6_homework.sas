DATA soy;
INPUT block  geno $ leaflet Area;
CARDS;
  1    Hendrick     3         62.74
  2    Hendrick     3         71.07
  3    Hendrick     3         84.76
  4    Hendrick     3         90.33
  1    Hendrick     7         54.55
  2    Hendrick     7         50.25
  3    Hendrick     7         90.42
  4    Hendrick     7        103.55
  1    Mn1401       3         66.27
  2    Mn1401       3        104.37
  3    Mn1401       3        106.57
  4    Mn1401       3         66.76
  1    Mn1401       7         99.00
  2    Mn1401       7        109.16
  3    Mn1401       7        127.35
  4    Mn1401       7        101.38
  1    Mn1801       3        119.45
  2    Mn1801       3        122.32
  3    Mn1801       3        110.41
  4    Mn1801       3         79.06
  1    Mn1801       7        148.14
  2    Mn1801       7        153.89
  3    Mn1801       7        123.71
  4    Mn1801       7        101.13
  1    Traill       3         48.33
  2    Traill       3         66.59
  3    Traill       3         47.93
  4    Traill       3         62.69
  1    Traill       7         61.62
  2    Traill       7         76.86
  3    Traill       7         57.95
  4    Traill       7         96.32
;
 
 /* Code for students to complete */
 TITLE 'conditional model: ';
 PROC GLIMMIX DATA = soy NOBOUND PLOTS = STUDENTPANEL(blup);
 	CLASS <?>;
 	MODEL Area = <?> / DDFM=KENWARDROGER;
 	RANDOM <?>;
	/* Goal: test for the effect of leaflet for each level of geno */
	LSMEANS geno*leaflet / PLOT = meanplot(sliceby=leaflet join cl);
RUN;

 TITLE 'conditional model: ';
 PROC GLIMMIX DATA = soy NOBOUND PLOTS = STUDENTPANEL(blup) outdesign=matrix;
 	CLASS block geno leaflet;
 	MODEL Area = geno leaflet geno*leaflet / ddfm=kr;
 	RANDOM block / SUBJECT=block V;
	LSMEANS geno*leaflet /  PLOT = meanplot(sliceby=leaflet join cl);
RUN;

PROC PRINT DATA=matrix;
RUN;

 TITLE 'marginal model: ';
 PROC GLIMMIX DATA = soy NOBOUND PLOTS = STUDENTPANEL(blup) outdesign=matrix;
 	CLASS block geno leaflet;
 	MODEL Area = geno leaflet geno*leaflet / ddfm=kr;
 	RANDOM _residual_ / TYPE = cs SUBJECT = block V;
	LSMEANS geno*leaflet /  PLOT = meanplot(sliceby=leaflet join cl);
RUN;

PROC PRINT DATA=matrix;
RUN;
