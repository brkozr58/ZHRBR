
PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhrbr_v005 CURSOR nextline.
    MODULE liste_show_liste.
    MODULE set_text.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhrbr_v005-bukrs .
      FIELD zhrbr_v005-paypd .
      FIELD zhrbr_v005-lgart .
      FIELD zhrbr_v005-begda .
      FIELD zhrbr_v005-endda .
      FIELD zhrbr_v005-islem .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhrbr_v005-bukrs .
      FIELD zhrbr_v005-paypd .
      FIELD zhrbr_v005-lgart .
      FIELD zhrbr_v005-begda .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
