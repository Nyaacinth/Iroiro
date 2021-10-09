import {NarratorChoice, NarratorParagraph} from "./NarratorDataStructs"

/** Interface of a Narrator Story Instance */
export interface INarratorStory {
    /** Begins the story. Generates the first chunk of paragraphs and choices. */
    begin(): void

    /** Showing the story have paragraphs to output or not */
    canContinue(): boolean

    /**
     * Get the next paragraphs
     * @param steps The number of paragraphs that you want to pull. 0 and not given will get all the currently available paragraphs
     */
    continue(steps?: number): NarratorParagraph[]

    /** Showing the story have choices to output or not, also return false if there are available paragraphs to continue */
    canChoose(): boolean

    /** Get available choices in an array, return an empty array if there are available paragraphs to continue */
    getChoices(): NarratorChoice[]

    /**
     * Make a choice to continue the story. Will do nothing when `canContinue()` returns `false`
     * @param index The choice that you was received with `getChoices()` before
     */
    choose(index: number): void

    /**
     * Jump to the path
     * @param path Knot Path
     * @example
     * ```
     * "knot.stitch.label"
     * ```
     */
    jumpTo(path: string): void

    /**
     * Get the number of visits to the path
     * @param path Knot Path
     * @example
     * ```
     * "knot.stitch.label"
     * ```
     */
    getVisits(path: string): number

    /**
     * Get tags for the path
     * @param path Knot Path
     * @example
     * ```
     * "knot.stitch.label"
     * ```
     */
    getTags(path: string): string[]

    /**
     * Make an observer function called when the variable changes
     * @param variable_name Name of Variable
     * @param onChange Change Callback
     */
    observe(variable_name: string, onChange: (this: void, value: unknown) => void): void

    /**
     * Bind a function to extend calling from the Ink
     * @param func_name Function name in Ink
     * @param handler Handler, receives unknown arguments then return values (can be void)
     */
    bind(func_name: string, handler: (this: void, ...vargs: unknown[]) => any): void
}

/** Narrator Story Instance */
export type NarratorStory = INarratorStory & {
    /** An array contains book's global tags */
    globalTags: string[]

    /** Book's constants */
    readonly constants: {[const_name: string]: unknown}

    /** Book's variables, can be read and write */
    variables: {[var_name: string]: unknown}
}
