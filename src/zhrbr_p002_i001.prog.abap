*&---------------------------------------------------------------------*
*& Include          ZETPA_P029_I001
*&---------------------------------------------------------------------*
* Tables ..
TABLES : sscrfields,
         pernr, pa0001.

INFOTYPES : 0000,0001,9950,9951,0041,0770.

CLASS lcl_report DEFINITION DEFERRED.

DATA  gr_report TYPE REF TO lcl_report.
*
** alv datas
DATA : gt_report TYPE TABLE OF zhrbr_s003,
       gt_t006   TYPE TABLE OF zhrbr_t006,
       ls_t006   TYPE zhrbr_t006,
       gv_begda  TYPE begda,
       gv_endda  TYPE endda.

"--->add code  by MKARABULUT ~ begin
DATA: gt_t591s TYPE TABLE OF t591s,
      gs_t591s TYPE          t591s.
"--<<add code  by MKARABULUT ~ end

DATA : t591s TYPE TABLE OF t591s .
data :
       T001P    type T001P    occurs 0 WITH HEADER LINE ,
       hrp1000  type hrp1000  occurs 0 WITH HEADER LINE ,
       T549T    type T549T    occurs 0 WITH HEADER LINE .
