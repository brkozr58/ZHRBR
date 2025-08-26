*&---------------------------------------------------------------------*
*&  Include           ZHRBR_P003_I002
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
PARAMETERS: p_spmon TYPE s001-spmon DEFAULT sy-datum OBLIGATORY.
PARAMETERS: p_icram TYPE text255      ."OBLIGATORY.
PARAMETERS: p_bankn TYPE p0009-bankn  ."OBLIGATORY.
PARAMETERS: p_acklm TYPE text255      ."OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b1 .
