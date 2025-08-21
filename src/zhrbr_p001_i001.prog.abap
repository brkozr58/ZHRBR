*&---------------------------------------------------------------------*
*& Include          zhrbr_P001_I001
*&---------------------------------------------------------------------*
TABLES : sscrfields.
INCLUDE : icons .

CLASS lcl_report DEFINITION DEFERRED.

FIELD-SYMBOLS : <fs_report> TYPE zhrbr_s001.

DATA : gr_report TYPE REF TO lcl_report.

DATA : gt_report TYPE TABLE OF zhrbr_s001,
       gs_report TYPE zhrbr_s001.

DATA: gt_word TYPE TABLE OF zhrbr_s002, "word document datas
      gs_word TYPE zhrbr_s002.

DATA: save_ok   TYPE sy-ucomm,
      number(3) TYPE n VALUE '110'.

DATA: gt_9950 LIKE TABLE OF pa9950,
      gs_9950 LIKE LINE OF gt_9950.

DATA : gv_error.
