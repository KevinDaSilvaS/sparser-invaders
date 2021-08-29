import Unit.Src.OutputFormatter.ColorsSpec ( colorsSpec )
import Unit.Src.OutputFormatter.FormatMessageSpec ( formatMessageSpec )
import Unit.Src.Transpiler.IRBuilderSpec ( iRBuilderSpec )
import Unit.Src.Transpiler.HelpersSpec ( helpersSpec )
import Unit.Src.Transpiler.LexicalAnalyserSpec ( getTokenSpec )

main :: IO ()
main = do
    colorsSpec
    formatMessageSpec
    iRBuilderSpec
    helpersSpec
    getTokenSpec
