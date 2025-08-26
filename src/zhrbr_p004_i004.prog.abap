*&---------------------------------------------------------------------*
*& Include          ZHRBR_P004_I004
*&---------------------------------------------------------------------*
INITIALIZATION.
  CREATE OBJECT gr_report.
  gr_report->set_init( ).

AT SELECTION-SCREEN.
  gr_report->at_selection_screen( ).

 AT SELECTION-SCREEN OUTPUT.
   gr_report->modify_screen( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  gr_report->screen_output( ).

START-OF-SELECTION.
if rb_ins eq abap_true.
  gr_report->start_of_selection( ).
ELSEIF rb_del eq abap_true.
  gr_report->get_data_del( ).
  ENDIF.

END-OF-SELECTION.
  gr_report->set_texts( ).
  gr_report->prepare_alv( ).
