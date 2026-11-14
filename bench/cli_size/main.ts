const args: string[] = process.argv.slice(2)
const index: number = args.indexOf("--name")
const name: string = index === -1 ? "Yack" : args[index + 1]
console.log(`Hello from ${name} !`)
