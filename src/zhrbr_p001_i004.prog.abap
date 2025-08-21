*&---------------------------------------------------------------------*
*& Include          ZHRIC_P001_I004
*&---------------------------------------------------------------------*
INITIALIZATION.
  CREATE OBJECT gr_report.

AT SELECTION-SCREEN OUTPUT.

  gr_report->at_selection_screen( ) .

START-OF-SELECTION.

  IF p_dosno IS NOT INITIAL .
    gr_report->get_data( ) .
  ENDIF .

  CALL SCREEN 100.

END-OF-SELECTION.
