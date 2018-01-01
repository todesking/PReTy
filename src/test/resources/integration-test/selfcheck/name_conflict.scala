trait A {
  def f = 1
  def f = 2
  //  ^ method f is defined twice;\n  the conflicting method f was defined at line 2:7
}
