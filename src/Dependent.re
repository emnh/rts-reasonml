/* Example */
module Three = {
  let getObjectMatrixCache = ref(None);
  let getObjectMatrix = (x, y, z) => {
    let args = (x, y, z);
    switch getObjectMatrixCache^ {
    | Some((retval, savedArgs)) when savedArgs == args => retval
    | _ =>
      let retval = Three.getObjectMatrix(x, y, z);
      getObjectMatrixCache := Some((retval, args));
      retval;
    };
  };
};
