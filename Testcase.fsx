

type Base() =
    abstract member Method : 't -> unit
    default this.Method(t) = ()

and Derived() =
    inherit Base()
    override this.Method(t) = base.Method(t)


 
