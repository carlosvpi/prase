// WS     = /[\ \n\t]/
//        | '(*' NoCloseComment '*)'
// WSs    = { WS }
// NT     = /[a-zA-Z_][a-zA-Z0-9_]*/
// Rule   = NT '=' WSs Exp ';' WSs
// Exp    = Disj { '|' WSs Disj }
// Disj   = Xcep ['-' WSs Xcep]
// Xcep   = Conc { Conc }
// Conc   = /[0-9]+/ WSs '*' WSs NT
//        | NT
//        | '"' NoDblQuote '"' WSs
//        | "'" NoSglQuote "'" WSs
//        | '/' NoSlash '/' WSs
//        | '[' Exp ']' WSs
//        | '{' Exp '}' WSs
//        | '(' Exp ')' WSs
//        | '?' NoQuestion '?' WSs

// type Rule = (Input, Config) -> [Node, Input]
// type Config = (original: Input, special: Maybe Rule, recover: Maybe (Input -> Input))

const REQUIRE = (error, rule) => (input, config) => {
  const match = rule(input, config)
  if (!match || !match[0]) {
    const errorString = error(input, config)
    const { original, recover } = config
    const indexOfError = input === ''
      ? original.length - 1
      : original.indexOf(input)
    const linesAtError = original.slice(0, indexOfError + 1).split('\n')
    const linesBeforeError = linesAtError.slice(0, linesAtError)
    const lineOfError = linesBeforeError.length
    const cursorOfError = indexOfError - linesBeforeError.reduce((acc, line) => acc + line.length, 0) + 1
    const errorPadding = Array(cursorOfError).fill().map(() => ' ').join('')
    console.error(`${errorString}
${original.split('\n')[lineOfError]}
${errorPadding}^
At ${lineOfError + 1}:${cursorOfError + 1}`)
    const recoveredInput = recover ? recover(input) : input
    return [null, input]
  } else {
    return match
  }
}
const EXPECT = (expected) => (input, config) => {
  const got = input === ''
    ? 'end of input'
    : `'${input.slice(0, 10).split("\n").join("\\n")}...'`
  return `Expected '${expected}'; got ${got}`
}

const CONCAT = (...rules) => (input, config) => {
  const originalInput = input
  const result = {
    type: 'concat',
    concat: []
  }
  let node, rest, returned
  for (let i = 0; i < rules.length; i++) {
    returned = rules[i](input, config)
    node = returned[0]
    rest = returned[1]
    if (!node) {
      return [null, originalInput]
    } else {
      input = rest
      result.concat.push(node)
    }
  }
  return [result, input]
}

const DISJUNCTION = (...rules) => (input, config) => {
  const result = {
    type: 'disjunction',
    disjunction: null
  }
  let node, rest, returned
  for (let i = 0; i < rules.length; i++) {
    returned = rules[i](input, config)
    node = returned[0]
    rest = returned[1]
    if (node) {
      result.disjunction = node
      return [result, rest]
    }
  }
  return [null, input]
}

const OPTION = rule => (input, config) => {
  const result = {
    type: 'option',
    option: null
  }
  const [node, rest] = rule(input, config)
  if (node) {
    input = rest
    result.option = node
  }
  return [result, input]
}

const CLOSURE = rule => (input, config) => {
  const result = {
    type: 'closure',
    closure: []
  }
  let [node, rest] = rule(input, config)
  let returned
  while (node) {
    input = rest
    result.closure.push(node)
    returned = rule(input, config)
    rest = returned[1]
    node = returned[0]
  }
  return [result, input]
}

const REPETITION = (times, rule) => (input, config) => {
  const result = {
    type: 'repetition',
    repetition: []
  }
  let node, rest, returned
  for (let i = 0; i < times; i++) {
    returned = rule(input, config)
    node = returned[0]
    rest = returned[1]
    if (!node) {
      return [null, input]
    }
    input = rest
    result.repetition.push(node)
  }
  return [result, input]
}

const EXCEPTION = (rule, exceptRule) => (input, config) => {
  const result = {
    type: 'exception',
    exception: null
  }
  const [node, rest] = rule(input, config)
  const [exceptNode] = exceptRule(input, config)
  if (!node || exceptNode) {
    return [null, input]
  }
  result.exception.push(node) 
  return node
}

const LITERAL = literal => (input, config) => {
  const result = {
    type: 'literal',
    literal
  }
  if (input.indexOf(literal) === 0) {
    return [result, input.slice(literal.length)]
  }
  return [null, input]
}

const MATCH = regex => (input, config) => {
  const result = {
    type: 'match',
    match: null
  }
  const match = input.match(regex)
  if (!match || match.index !== 0) {
    return [null, input]
  } else {
    result.match = match
    return [result, input.slice(match[0].length)]
  }
}
const WS = DISJUNCTION(
  MATCH(/[\ \n\t]/),
  CONCAT(
    LITERAL('(*'),
    MATCH(/(?!(\*\)))*/),
    REQUIRE(EXPECT('*)'), LITERAL('*)'))
  )
)
const WSs = CLOSURE(WS)
const NT = CONCAT(MATCH(/[a-zA-Z_][a-zA-Z0-9_]*/), WSs)
const Conc = (input, config) => DISJUNCTION(
  CONCAT(
    MATCH(/[0-9]+/),
    WSs,
    REQUIRE(EXPECT('*'), LITERAL('*')),
    WSs,
    NT
  ),
  NT,
  CONCAT(
    LITERAL('"'),
    MATCH(/[^\"]/),
    REQUIRE(EXPECT('"'), LITERAL('"')),
    WSs
  ),
  CONCAT(
    LITERAL("'"),
    MATCH(/[^\']/),
    REQUIRE(EXPECT("'"), LITERAL("'")),
    WSs
  ),
  CONCAT(
    LITERAL('/'),
    MATCH(/[^\/]/),
    REQUIRE(EXPECT('/'), LITERAL('/')),
    WSs
  ),
  CONCAT(
    LITERAL('['),
    WSs,
    Exp,
    REQUIRE(EXPECT(']'), LITERAL(']')),
    WSs
  ),
  CONCAT(
    LITERAL('{'),
    WSs,
    Exp,
    REQUIRE(EXPECT('}'), LITERAL('}')),
    WSs
  ),
  CONCAT(
    LITERAL('('),
    WSs,
    Exp,
    REQUIRE(EXPECT('('), LITERAL(')')),
    WSs
  ),
  CONCAT(
    LITERAL('?'),
    config.special || (x => x),
    REQUIRE(EXPECT('?'), LITERAL('?')),
    WSs
  )
)(input, config)
const Xcep = CONCAT(Conc, CLOSURE(Conc))
const Disj = CONCAT(
  Xcep,
  OPTION(
    CONCAT(
      LITERAL('-'),
      WSs,
      REQUIRE(EXPECT('Expression after "-"'), Xcep)
    )
  )
)
const Exp = CONCAT(
  Disj,
  CLOSURE(
    CONCAT(
      LITERAL('|'),
      WSs,
      REQUIRE(EXPECT('Expression after "|"'), Disj)
    )
  )
)
const Rule = CONCAT(
  NT,
  REQUIRE(EXPECT('='), LITERAL('=')),
  WSs,
  REQUIRE(EXPECT('Expression after "="'), Exp),
  REQUIRE(EXPECT(';'), LITERAL(';')),
  WSs
)
const EBNF = CLOSURE(Rule)

// type prase = (special: Maybe Rule) -> Input -> Node
const prase = ({ special = (x => x), recover = (x => x), axiom = 'S' } = {}) => input => {
  const [ebnf, rest] = EBNF(input, { special, original: input, recover })
  if (rest) {
    console.warning(`There is some trailing data in input: ${rest}`)
  }
  const grammar = buildGrammar(ebnf)
  return grammar[axiom]
}
const buildGrammar = ({ closure }) => closure.reduce((acc, rule) => {
  debugger
  const { head, body } = getRule(rule)
  acc[head] = body
  return acc
}, {})
const getRule = ({ concat }) => ({
  head: concat[0].concat[0].match[0],
  body: getExp(concat[3].concat)
})
const getExp = ([disj,  { closure }]) => DISJUNCTION(getDisj(disj), ...closure.map(getDisj))
const getDisj = ([xcep, { option }]) => EXCEPTION(getXcep(xcep), getXcep(option))
const getXcep = ({ })

if (typeof module !== 'undefined') {
  module.exports = prase
} else if (typeof window !== 'undefined') {
  window.prase = prase
}