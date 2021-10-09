/** Narrator Choice */
export type NarratorChoice = {
    /** Choice Label */
    text: string

    /** Choice Tags, non-standard feature */
    tags?: string[]
}

/** Narrator Paragraph */
export type NarratorParagraph = {
    /** Paragraph Text */
    text: string

    /** Paragraph Tags */
    tags?: string[]
}
