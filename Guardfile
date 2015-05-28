guard :shell do
    watch(%r{first_steps.hs$}) do
        system('rm -f a.out')
        system('ghc *.hs -o a.out')
        system('./a.out Ben Inna')
        `date`
    end

    watch(%r{simple_parser.hs$}) do
        system('rm -f a.out')
        system('ghc --make simple_parser.hs -o a.out && ./test.sh')
        `date`
    end

    watch(%r{test.sh$}) do
        system('./test.sh')
        `date`
    end
end
