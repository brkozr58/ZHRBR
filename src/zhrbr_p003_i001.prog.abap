*&---------------------------------------------------------------------*
*&  Include           ZHRBR_P003_I001
*&---------------------------------------------------------------------*
* Tables ..
TABLES : s001, sscrfields,
         pernr.

INFOTYPES : 0000,0001,9950,9951.
CLASS lcl_report DEFINITION DEFERRED.


CONSTANTS: gc_newline(1) VALUE cl_abap_char_utilities=>newline,
           gc_space(1)   VALUE cl_abap_char_utilities=>horizontal_tab.
DATA  gr_report TYPE REF TO lcl_report.

DATA : gt_report TYPE TABLE OF zhrbr_s004,
       gs_report TYPE          zhrbr_s004,
       gs_t001  TYPE  t001,
       gt_t001  TYPE TABLE OF t001,
       gt_t006   TYPE TABLE OF zhrbr_t006,
       ls_t006   TYPE zhrbr_t006,
       gv_begda  TYPE begda,
       gv_endda  TYPE endda.

"data definitions of OLE objects with type ole2_object
DATA: g_tmp_workbook TYPE ole2_object.
DATA: g_tmp_worksheets TYPE ole2_object.
DATA: g_tmp_worksheet1 TYPE ole2_object.
DATA: g_tmp_worksheet2 TYPE ole2_object.

DATA: g_pc_template LIKE rcgfiletr-ftappl
VALUE 'd:\temp\mmr_rpt_tsl_tmp.xls'.
DATA: g_excel TYPE ole2_object.
DATA: g_column TYPE ole2_object.
DATA: g_workbooks TYPE ole2_object.
DATA: g_workbook TYPE ole2_object.
DATA: g_worksheets TYPE ole2_object.
DATA: g_worksheet TYPE ole2_object.
DATA: g_cell TYPE ole2_object.
DATA: g_cell1 TYPE ole2_object.
DATA: g_cell2 TYPE ole2_object.
DATA: g_cellrange TYPE ole2_object.
DATA: g_font TYPE ole2_object.
DATA: g_interior TYPE ole2_object.
DATA: g_borders TYPE ole2_object.
DATA: g_first_ws VALUE 'Y'.
DATA: g_row TYPE i.
DATA: g_col TYPE i.
"data definitions of OLE objects with type ole2_object
