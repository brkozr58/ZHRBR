*&---------------------------------------------------------------------*
*& Include          ZETPA_P029_I002
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: s_datum FOR sy-datum
                     OBLIGATORY NO-EXTENSION.
SELECT-OPTIONS: s_subty FOR p9950-subty NO INTERVALS
                     MATCHCODE OBJECT zhrbr_sh003.
PARAMETERS    : p_fin AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK b1 .
