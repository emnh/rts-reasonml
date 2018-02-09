type event;

external getClientX : event => int = "clientX" [@@bs.get];
external getClientY : event => int = "clientY" [@@bs.get];
