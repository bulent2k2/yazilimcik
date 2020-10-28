// 39 + 12    From: https://www.geeksforgeeks.org/scala-keywords/   
val keywords = List("abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield")
val symbols = List(">:", "⇒", "=>", "=", "<%", "<:", "←", "<-", "#", "@", ":", "_")
val k1 = keywords.mkString(" ")
val k2 = symbols.mkString(" ")
println(s"There are ${keywords.size} keywords with only letters. They are: {$k1}")
println(s"There are ${symbols.size} symbols reserved as keywords, too. They are: {$k2}")
