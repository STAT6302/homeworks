DATA alcohol;                                                                                                                           
	length alcoholic $ 15;                                                                                                                  
	input Subject      Metabol      Gastric      gender $      alcoholic $;                                                                    
	DATAlines;                                                                                                                              
	1      0.6      1      Female      alcoholic                                                                                            
	2      0.6      1.6      Female      alcoholic                                                                                          
	3      1.5      1.5      Female      alcoholic                                                                                          
	4      0.4      2.2      Female      Non-alcoholic                                                                                      
	5      0.1      1.1      Female      Non-alcoholic                                                                                      
	6      0.2      1.2      Female      Non-alcoholic                                                                                      
	7      0.3      0.9      Female      Non-alcoholic                                                                                      
	8      0.3      0.8      Female      Non-alcoholic                                                                                      
	9      0.4      1.5      Female      Non-alcoholic                                                                                      
	10      1      0.9      Female      Non-alcoholic                                                                                       
	11      1.1      1.6      Female      Non-alcoholic                                                                                     
	12      1.2      1.7      Female      Non-alcoholic                                                                                     
	13      1.3      1.7      Female      Non-alcoholic                                                                                     
	14      1.6      2.2      Female      Non-alcoholic                                                                                     
	15      1.8      0.8      Female      Non-alcoholic                                                                                     
	16      2      2      Female      Non-alcoholic                                                                                         
	17      2.5      3      Female      Non-alcoholic                                                                                       
	18      2.9      2.2      Female      Non-alcoholic                                                                                     
	19      1.5      1.3      Male      alcoholic                                                                                           
	20      1.9      1.2      Male      alcoholic                                                                                           
	21      2.7      1.4      Male      alcoholic                                                                                           
	22      3      1.3      Male      alcoholic                                                                                             
	23      3.7      2.7      Male      alcoholic                                                                                           
	24      0.3      1.1      Male      Non-alcoholic                                                                                       
	25      2.5      2.3      Male      Non-alcoholic                                                                                       
	26      2.7      2.7      Male      Non-alcoholic                                                                                       
	27      3      1.4      Male      Non-alcoholic                                                                                         
	28      4      2.2      Male      Non-alcoholic                                                                                         
	29      4.5      2      Male      Non-alcoholic                                                                                         
	30      6.1      2.8      Male      Non-alcoholic                                                                                       
	31      9.5      5.2      Male      Non-alcoholic                                                                                       
	32      12.3      4.1      Male      Non-alcoholic                                                                                      
;                                                                                                                                       

/* Step 0: Recode Variables so we can use PROC REG;*/
DATA alcohol;                                                                                                                           
	set alcohol;                                                                                                                            
	if gender eq "Female" then genderC = 1;                                                                                                       
	if gender eq "Male" then genderC = 0;                                                                                                         
	if alcoholic eq "alcoholic" then alcoholicC = 1;                                                                                        
	if alcoholic eq "Non-alcoholic" then alcoholicC = 0;                                                                                    
	if alcoholic eq 'Non-alcoholic' and gender eq 'Male' then Cross = 'MNA';                                                                   
	if alcoholic eq 'Non-alcoholic' and gender eq 'Female' then Cross = 'FNA';                                                                 
	if alcoholic eq 'alcoholic' and gender eq 'Male' then Cross = 'MA';                                                                        
	if alcoholic eq 'alcoholic' and gender eq 'Female' then Cross = 'FA';                                                                      
;                                                                                                 
/* need to code interaction terms */                                                                                                    
DATA alcohol;                                                                                                                        
	set alcohol;                                                                                                                            
	GF = Gastric * genderC;                                                                                                                    
	GA = Gastric * alcoholicC;                                                                                                              
	GAF = genderC * alcoholicC * Gastric;                                                                                                      
	FA = genderC * alcoholicC;                                                                                                                 
RUN;      

/* Step 1 Plot DATA with Scatterplots */                                                                                                

symbol1 C=red V=circle i=r H=0.8;
symbol2 C=blue V=circle  i=r H=0.8;
symbol3 C=red V=plus i=r H=0.8;
symbol4 C=blue V=plus  i=r H=0.8;
axis1 LABEL=(r=0 a=90) MINOR=none;
axis2 MINOR=none;
PROC GPLOT DATA = alcohol;
	PLOT Metabol*Gastric=Cross / VAXIS=axis1 HAXIS=axis2;

PROC SGSCATTER DATA = alcohol;                                                                                                          
	MATRIX  metabol Gastric  / GROUP=Cross DATALABEL = Subject;                                                                        
	RUN;                                                                                                                                    

/* QOI 1: Metabolism different by gender? */
PROC GLM DATA = alcohol;
CLASS gender ;
MODEL metabol =  gender  / solution;                                                                                                                                                                    
RUN;                                                                                                                                    
 
PROC TTEST DATA = alcohol;
CLASS gender;
var Metabol;
RUN;

PROC NPAR1WAY DATA = alcohol WILCOXON;
	CLASS gender;
	VAR metabol;
	EXACT HL;
RUN;

/*QOI 2 Do differences in gender persist when controlling for gastric? */
PROC GLM DATA = alcohol PLOTS = all;
	CLASS gender(ref = "Male");
	MODEL metabol = gastric|gender / SOLUTION;
	output out = output student=res h = lev cookd = cookd;
RUN;

PROC REG DATA = alcohol PLOTS = dfbetas;                                                                                                             
	MODEL metabol = gastric genderC GF; 
RUN;                                                                                                                                    

PROC SGPLOT DATA = output;
	SCATTER x = lev y = res /  DATALABEL = subject;
RUN;

PROC SGPLOT DATA = output;
	SCATTER x = subject y = cookd /  DATALABEL = subject;
RUN;

/* Continuing without high leverage points */

PROC GLM DATA = alcohol PLOTS = all;
	WHERE gastric <= 3;
	CLASS gender(ref = "Male");
	MODEL metabol = gastric|gender / SOLUTION;
	output out = output student=res h = lev cookd = cookd;
RUN;

*eliminate interaction;
PROC GLM DATA = alcohol PLOTS = all;
	WHERE gastric <= 3;
	CLASS gender(ref = "Male");
	MODEL metabol = gastric gender / SOLUTION;
	output out = output student=res h = lev cookd = cookd;
RUN;


/*QOI 3 Do we need to account for alcoholism?*/
PROC GLM DATA = alcohol PLOTS = all;
	WHERE gastric <= 3;
	CLASS gender(ref = "Male");
	MODEL metabol = gastric|gender / SOLUTION;
	output out = output student=res h = lev cookd = cookd;
RUN;

PROC GLM DATA = alcohol PLOTS = all;
	WHERE gastric <= 3;
	CLASS gender(ref = "Male") alcoholic;
	MODEL metabol = gastric|gender|alcoholic / SOLUTION;
	output out = output student=res h = lev cookd = cookd;
RUN;


*Rcode: 1-pf( (20.22-19.48)/(4)/(19.48/22),4,22);
*[1] 0.9307025;