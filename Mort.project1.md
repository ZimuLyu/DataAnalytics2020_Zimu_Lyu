
``` r
rm(list=ls()) 
setwd("/Users/cleardog/BA/MGMT 6962 Adv AI:ML for Finance/Project 1")
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(plm)
```

    ## 
    ## Attaching package: 'plm'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, lead

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(rpart)
library(reshape)
```

    ## 
    ## Attaching package: 'reshape'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     rename

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, smiths

``` r
library(knitr)
load("Mort.Sample.Group3.Rda")

df <- mort.group.3
rm(mort.group.3)
head(df)
```

    ##         V1      LOAN_ID ORIG_CHN Seller.Name ORIG_RT ORIG_AMT ORIG_TRM ORIG_DTE
    ## 1:  154069 420609590121        R       OTHER   2.875   450000      180  08/2016
    ## 2: 2682459 846895888381        R       OTHER   5.500    30000      120  08/2003
    ## 3:  247415 610668057421        C       OTHER   3.875   115000      360  02/2016
    ## 4:  381502 244371124021        R       OTHER   5.750    88000      360  03/2004
    ## 5:    3543 107428173821        R       OTHER   3.500   290000      360  07/2016
    ## 6:  598461 506589953281        R       OTHER   4.500   140000      360  07/2013
    ##    FRST_DTE OLTV OCLTV NUM_BO DTI CSCORE_B FTHB_FLG PURPOSE PROP_TYP NUM_UNIT
    ## 1:  11/2016   19    19      1  28      802        N       R       SF        1
    ## 2:  10/2003   13    13    2.0  11      758        N       P       SF        1
    ## 3:  04/2016   85    85      1  31      751        Y       P       SF        1
    ## 4:  05/2004   90    90      2  26      773        N       P       SF        1
    ## 5:  09/2016   90    90      1  45      792        Y       P       SF        1
    ## 6:  09/2013   60    60      1  43      784        N       C       SF        1
    ##    OCC_STAT STATE ZIP_3 MI_PCT Product.Type CSCORE_C MI_TYPE RELOCATION_FLG
    ## 1:        P    CA   943     NA          FRM       NA      NA              N
    ## 2:        P    MD   207     NA          FRM      785      NA              N
    ## 3:        P    KS   662     12          FRM       NA       1              N
    ## 4:        I    TX   781     25          FRM      762       1              N
    ## 5:        P    MA    26     25          FRM       NA       1              N
    ## 6:        P    LA   711     NA          FRM       NA      NA              N
    ##    Monthly.Rpt.Prd Servicer.Name LAST_RT  LAST_UPB Loan.Age Months.To.Legal.Mat
    ## 1:      2016-12-01                 2.875        NA        2                 178
    ## 2:      2003-12-01                 5.500        NA        3                 117
    ## 3:      2016-12-01                 3.875 113208.71        9                 351
    ## 4:      2004-12-01                 5.750  86979.02        8                 352
    ## 5:      2016-12-01                 3.500        NA        4                 356
    ## 6:      2013-12-01                 4.500        NA        4                 356
    ##    Adj.Month.To.Mat Maturity.Date   MSA Delq.Status MOD_FLAG Zero.Bal.Code
    ## 1:              175       10/2031 41940           0        N              
    ## 2:              101       09/2013 12580           0        N              
    ## 3:              351       03/2046 28140           0        N              
    ## 4:              352       04/2034 41700           0        N              
    ## 5:              354       08/2046 12700           0        N              
    ## 6:              356       08/2043 43340           0        N              
    ##    ZB_DTE LPI_DTE FCC_DTE DISP_DT FCC_COST PP_COST AR_COST IE_COST TAX_COST
    ## 1:                   <NA>    <NA>       NA      NA      NA      NA       NA
    ## 2:                   <NA>    <NA>       NA      NA      NA      NA       NA
    ## 3:                   <NA>    <NA>       NA      NA      NA      NA       NA
    ## 4:                   <NA>    <NA>       NA      NA      NA      NA       NA
    ## 5:                   <NA>    <NA>       NA      NA      NA      NA       NA
    ## 6:                   <NA>    <NA>       NA      NA      NA      NA       NA
    ##    NS_PROCS CE_PROCS RMW_PROCS O_PROCS NON_INT_UPB REPCH_FLAG TRANSFER_FLAG
    ## 1:       NA       NA        NA      NA          NA                        N
    ## 2:       NA       NA        NA      NA          NA                         
    ## 3:       NA       NA        NA      NA          NA                        N
    ## 4:       NA       NA        NA      NA          NA                         
    ## 5:       NA       NA        NA      NA          NA                        N
    ## 6:       NA       NA        NA      NA          NA                        N
    ##    CSCORE_MN   ORIG_VAL PRIN_FORG_UPB MODTRM_CHNG MODUPB_CHNG   Fin_UPB
    ## 1:       802 2368421.05            NA           0           0      0.00
    ## 2:       758  230769.23            NA           0           0      0.00
    ## 3:       751  135294.12            NA           0           0 113208.71
    ## 4:       762   97777.78            NA           0           0  86979.02
    ## 5:       792  322222.22            NA           0           0      0.00
    ## 6:       784  233333.33            NA           0           0      0.00
    ##    modfg_cost C_modir_cost C_modfb_cost Count LAST_STAT lpi2disp zb2disp
    ## 1:          0            0            0     4         C        0       0
    ## 2:          0            0            0     3         C        0       0
    ## 3:          0            0            0     9         C        0       0
    ## 4:          0            0            0     9         C        0       0
    ## 5:          0            0            0     5         C        0       0
    ## 6:          0            0            0     5         C        0       0
    ##    INT_COST total_expense total_proceeds NET_LOSS NET_SEV Total_Cost Tot_Procs
    ## 1:        0             0              0        0       0          0         0
    ## 2:        0             0              0        0       0          0         0
    ## 3:        0             0              0        0       0          0         0
    ## 4:        0             0              0        0       0          0         0
    ## 5:        0             0              0        0       0          0         0
    ## 6:        0             0              0        0       0          0         0
    ##    Tot_Liq_Ex   LAST_DTE FMOD_DTE FMOD_UPB FCE_DTE FCE_UPB F180_DTE F180_UPB
    ## 1:          0 2016-12-01     <NA>       NA    <NA>      NA     <NA>       NA
    ## 2:          0 2003-12-01     <NA>       NA    <NA>      NA     <NA>       NA
    ## 3:          0 2016-12-01     <NA>       NA    <NA>      NA     <NA>       NA
    ## 4:          0 2004-12-01     <NA>       NA    <NA>      NA     <NA>       NA
    ## 5:          0 2016-12-01     <NA>       NA    <NA>      NA     <NA>       NA
    ## 6:          0 2013-12-01     <NA>       NA    <NA>      NA     <NA>       NA
    ##    VinYr ActYr     DispYr MODIR_COST MODFB_COST MODTOT_COST       date n n.obs
    ## 1:  2016  2016 NO DISP_DT          0          0           0 2016-12-01 4    13
    ## 2:  2003  2003 NO DISP_DT          0          0           0 2003-12-01 3    22
    ## 3:  2016  2016 NO DISP_DT          0          0           0 2016-12-01 9    18
    ## 4:  2004  2004 NO DISP_DT          0          0           0 2004-12-01 9   106
    ## 5:  2016  2016 NO DISP_DT          0          0           0 2016-12-01 5    14
    ## 6:  2013  2013 NO DISP_DT          0          0           0 2013-12-01 5    38
    ##    year n.year n.year.max def.0 def.1 def.2 def.3 def.4 def.5
    ## 1: 2016      4          4     0     0     0     0     0     0
    ## 2: 2003      3          3     0     0     0     0     0     0
    ## 3: 2016      9          9     0     0     0     0     0     0
    ## 4: 2004      9          9     0     0     0     0     0     0
    ## 5: 2016      5          5     0     0     0     0     0     0
    ## 6: 2013      5          5     0     0     0     0     0     0

``` r
attach(df)
```

``` r
length(unique(df$V1))
```

    ## [1] 432168

``` r
length(unique(df$LOAN_ID))
```

    ## [1] 500000

``` r
hist(ORIG_RT)
```

![](Mort.project1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
