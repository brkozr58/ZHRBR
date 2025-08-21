*&---------------------------------------------------------------------*
*& Include          ZETPA_P029_I001
*&---------------------------------------------------------------------*
* Tables ..
TABLES : sscrfields,
         pernr, pa0001.

INFOTYPES : 0000,0001,9950,9951.

CLASS lcl_report DEFINITION DEFERRED.

DATA  gr_report TYPE REF TO lcl_report.
*
** alv datas
DATA : gt_report TYPE TABLE OF zhrbr_s003,
       gt_t006   TYPE TABLE OF zhrbr_t006,
       ls_t006   TYPE zhrbr_t006,
       gv_begda  TYPE begda,
       gv_endda  TYPE endda.
