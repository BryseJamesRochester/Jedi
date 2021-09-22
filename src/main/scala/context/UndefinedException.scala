package context

import expression.Identifier

class UndefinedException(name: Identifier) extends JediException("Undefined identifier: " + name)
