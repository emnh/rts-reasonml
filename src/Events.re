type event;

[@bs.get] external getClientX : event => int = "clientX";
[@bs.get] external getClientY : event => int = "clientY";