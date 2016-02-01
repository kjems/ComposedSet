namespace ComposedSet.Test

open NUnit.Framework

module UnitTest =
    open ComposedSet
    open ComposedSetOfStrings

    [<Test>]
    let GetHashCodeFS() =
        let abcd  = ComposedSet.decompose "A.B.C.D"
        let dcca  = ComposedSet.calchash << ComposedSet.decompose
        Assert.That(dcca "A.B.C.D" = dcca "A.B.C.D" , Is.True)
        Assert.That(ComposedSet.calchash abcd  = ComposedSet.calchash abcd  , Is.True)
        Assert.That(dcca ""        = dcca ""        , Is.True)
        Assert.That(dcca "A.B.C.D" = dcca "A.B.C.E" , Is.False)
        Assert.That(dcca " "       = dcca ""        , Is.False)
        
    [<Test>]
    let TrimEndFS() =
        let abcd  = ComposedSet.decompose "A.B.C.D"
        let ab    = ComposedSet.decompose "A.B."
        let cd    = ComposedSet.decompose "C.D"
        let empty = ComposedSet.decompose ""
        Assert.That(ComposedSet.equals (ComposedSet.trimend abcd cd) ab,         Is.True)
        Assert.That(ComposedSet.equals (ComposedSet.trimend ab ab) empty,        Is.True)
        Assert.That(ComposedSet.equals (ComposedSet.trimend empty empty) empty,  Is.True)
        Assert.That(ComposedSet.equals (ComposedSet.trimend ab empty) ab,        Is.True)
        Assert.That(ComposedSet.equals (ComposedSet.trimend abcd ab) ab,         Is.False)
        Assert.That(ComposedSet.equals (ComposedSet.trimend abcd ab) cd,         Is.False)
   
    [<Test>]
    let OperatorAddFS() =
        let (++)  = ComposedSet.concat
        let abcd  = ComposedSet.decompose "A.B.C.D"
        let a     = ComposedSet.decompose "A"
        let b     = ComposedSet.decompose "B"
        let c     = ComposedSet.decompose "C"
        let d     = ComposedSet.decompose "D"
        let ab    = ComposedSet.decompose "A.B"
        let cd    = ComposedSet.decompose "C.D"
        let dot   = ComposedSet.decompose "."
        Assert.That(ComposedSet.equals abcd (ab ++ dot ++ cd), Is.True)
        Assert.That(ComposedSet.equals (ab ++ dot ++ cd) (ab ++ dot ++ cd), Is.True)
        Assert.That(ComposedSet.equals abcd (a ++ dot ++ b ++ dot ++ c ++ dot ++ d), Is.True)
        Assert.That(abcd.GetHashCode() = (a ++ dot ++ b ++ dot ++ c ++ dot ++ d).GetHashCode(), Is.True)
        Assert.That(ComposedSet.equals abcd (ab ++ dot ++ dot ++ cd), Is.False)
 
    [<Test>]
    let EndsWithFS() =
        let dcew a b = ComposedSet.endswith (ComposedSet.decompose a) (ComposedSet.decompose b)
        Assert.That(dcew "A.B.C.D"     "A.B.C.D",Is.True)
        Assert.That(dcew "A.B.C.D"     "A.B.C.D",Is.True)
        Assert.That(dcew "B/A.C/A.C.D" "C.D",    Is.True) 
        Assert.That(dcew "0.B.C.D"     "C.D",    Is.True) 
        Assert.That(dcew "a.B.C.D"     "C.D",    Is.True)
        Assert.That(dcew "A/B/C/D"     "A.B.C.D",Is.False)
        Assert.That(dcew "A.B.CD"      "A.B.C.D",Is.False)
        Assert.That(dcew "A.B.C.C"     "A.B.C.D",Is.False)
        Assert.That(dcew "A.B.C.D."    "A.B.C.D",Is.False)
        Assert.That(dcew "0.B.C.D"     "A.B.C.D",Is.False)
        Assert.That(dcew "a.B.C.D"     "A.B.C.D",Is.False)
        Assert.That(dcew "B.B.C.D"     "A.B.C.D",Is.False) 
        Assert.That(dcew "A.B.C.D"     "C.D",    Is.True)
        Assert.That(dcew " "           "A.B.C.D",Is.False)
        Assert.That(dcew ""            "A.B.C.D",Is.False)

    [<Test>]
    let EqualsFS() =
        let dceq a b = ComposedSet.equals (ComposedSet.decompose a) (ComposedSet.decompose b)
        Assert.That(dceq "A.B.C.D"  "A.B.C.D", Is.True)
        Assert.That(dceq "A.B.C.D"  "A.B.C.D", Is.True)
        Assert.That(dceq " "        " ",       Is.True)
        Assert.That(dceq ""         "",        Is.True)
        Assert.That(dceq "A/B/C/D"  "A.B.C.D", Is.False)
        Assert.That(dceq "A.B.CD"   "A.B.C.D", Is.False)
        Assert.That(dceq "A.B.C.C"  "A.B.C.D", Is.False)
        Assert.That(dceq "A.B.C.C"  "A.B.C.D", Is.False)
        Assert.That(dceq "A.B.C.D." "A.B.C.D", Is.False)
        Assert.That(dceq "0.B.C.D"  "A.B.C.D", Is.False)
        Assert.That(dceq "a.B.C.D"  "A.B.C.D", Is.False)
        Assert.That(dceq "B.B.C.D"  "A.B.C.D", Is.False)
        Assert.That(dceq " "        "A.B.C.D", Is.False)
        Assert.That(dceq ""         "A.B.C.D", Is.False)

  
    [<Test>]
    let ComposeFS() =
        let dcc = ComposedSet.compose << ComposedSet.decompose
        Assert.That(dcc "A.B.C.D" , Is.EqualTo("A.B.C.D"))
        Assert.That(dcc "A.B.C.D" , Is.Not.EqualTo("A/B/C/D"))
        Assert.That(dcc "A/B/C/D" , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "A.B.CD"  , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "A.B.C.C" , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "A.B.C.D.", Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "0.B.C.D" , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "a.B.C.D" , Is.Not.EqualTo("A.B.C.D"))
        Assert.That(dcc "B.B.C.D" , Is.Not.EqualTo("A.B.C.D"))
       