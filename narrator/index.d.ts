import {NarratorStory} from "./NarratorStory"

/**
 * Narrator
 * @description The Ink language parser and runtime implementation in Lua
 * @link https://github.com/astrochili/narrator
 * @version 1.4
 * @author astrochili
 * @license MIT
 */
declare namespace narrator {
    /**
     * Load and parse the given file, then init a story instance from the result
     * @param path Inkscript File Path
     * @noSelf
     */
    function load(path: string): NarratorStory
}

export = narrator
