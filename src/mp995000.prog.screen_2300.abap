
PROCESS BEFORE OUTPUT.
  MODULE modify_subscreen.
*  CALL SUBSCREEN subscreen_empl   INCLUDING empl_prog empl_dynnr.
*  CALL SUBSCREEN subscreen_header INCLUDING header_prog header_dynnr.
*         infotype specific operations
  MODULE p9950_2100.
*
*  MODULE hidden_data.
*
PROCESS AFTER INPUT.
*---------------------------------------------------------------------*
*  process exit commands
*---------------------------------------------------------------------*
  MODULE exit AT EXIT-COMMAND.
*---------------------------------------------------------------------*
*         processing after input
*---------------------------------------------------------------------*
*
*         check and mark if there was any input: all fields that
*         accept input HAVE TO BE listed here
*---------------------------------------------------------------------*
  CHAIN.
    FIELD p9950-compl.
    FIELD p9950-aiban.
    FIELD p9950-alici.
    FIELD p9950-icrad.
    FIELD p9950-dosno.
    FIELD p9950-dossr.
    FIELD p9950-acilm.
    FIELD p9950-faizt.
    FIELD p9950-orny5.
    FIELD p9950-orng5.
    FIELD p9950-orny4.
    FIELD p9950-orng4.
    FIELD p9950-orny3.
    FIELD p9950-orng3.
    FIELD p9950-orny2.
    FIELD p9950-orng2.
    FIELD p9950-orny1.
    FIELD p9950-orng1.
    FIELD p9950-tkstt.
    FIELD p9950-tksts.
    FIELD p9950-waers.
    FIELD p9950-icrtr.
    FIELD p9950-hspsk.
    FIELD p9950-kessr.
    FIELD p9950-icrid.
    MODULE input_status ON CHAIN-REQUEST.
  ENDCHAIN.
*---------------------------------------------------------------------*
*      process functioncodes before input-checks                      *
*---------------------------------------------------------------------*
  MODULE pre_input_checks.
*---------------------------------------------------------------------*
*         input-checks:                                               *
*---------------------------------------------------------------------*

*   insert check modules here:

*  ...

*---------------------------------------------------------------------*
*     process function code: ALL fields that appear on the
*      screen HAVE TO BE listed here (including output-only fields)
*---------------------------------------------------------------------*
  CHAIN.
    FIELD p9950-compl.
    FIELD p9950-aiban.
    FIELD p9950-alici.
    FIELD p9950-icrad.
    FIELD p9950-dosno.
    FIELD p9950-dossr.
    FIELD p9950-acilm.
    FIELD p9950-faizt.
    FIELD p9950-orny5.
    FIELD p9950-orng5.
    FIELD p9950-orny4.
    FIELD p9950-orng4.
    FIELD p9950-orny3.
    FIELD p9950-orng3.
    FIELD p9950-orny2.
    FIELD p9950-orng2.
    FIELD p9950-orny1.
    FIELD p9950-orng1.
    FIELD p9950-tkstt.
    FIELD p9950-tksts.
    FIELD p9950-waers.
    FIELD p9950-icrtr.
    FIELD p9950-hspsk.
    FIELD p9950-kessr.
    FIELD p9950-icrid.
    MODULE post_input_checks.
  ENDCHAIN.
*
PROCESS ON VALUE-REQUEST.
  FIELD p9950-orng1 MODULE orng1_values.
  FIELD p9950-orng2 MODULE orng2_values.
  FIELD p9950-orng3 MODULE orng3_values.
  FIELD p9950-orng4 MODULE orng4_values.
  FIELD p9950-orng5 MODULE orng5_values.
