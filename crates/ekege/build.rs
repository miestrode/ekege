#![allow(missing_docs)]

use std::iter;

use rand::{seq::SliceRandom, Rng, SeedableRng};
use rand_chacha::ChaCha12Rng;
use rustifact::ToTokenStream;

const fn binomial_coefficient(mut n: u32, mut k: u32) -> u32 {
    let mut permutations = n;
    let mut orderings = k;

    while k > 1 {
        n -= 1;
        k -= 1;

        permutations *= n;
        orderings *= k;
    }

    permutations / orderings
}

const BLOOM_FILTER_TAG_ONE_BITS: u32 = 4;

const UNIQUE_BLOOM_FILTER_TAGS: u32 = binomial_coefficient(u16::BITS, BLOOM_FILTER_TAG_ONE_BITS);
const BLOOM_FILTER_TAGS: usize = UNIQUE_BLOOM_FILTER_TAGS.next_power_of_two() as usize;

const FIRST_BLOOM_FILTER_TAG: u16 = (1u16 << BLOOM_FILTER_TAG_ONE_BITS) - 1;
const LAST_BLOOM_FILTER_TAG: u16 = !0 << (u16::BITS - BLOOM_FILTER_TAG_ONE_BITS);

const RNG_SEED: u64 = 42;

fn get_rng() -> impl Rng {
    ChaCha12Rng::seed_from_u64(RNG_SEED)
}

fn write_bloom_filter_tags(rng: &mut impl Rng) {
    let mut bloom_filter_tags = iter::successors(Some(FIRST_BLOOM_FILTER_TAG), |&tag| {
        if tag == LAST_BLOOM_FILTER_TAG {
            return Some(FIRST_BLOOM_FILTER_TAG);
        }

        // This is a bit complicated, so I hope the descriptive names help.
        // Additionally, to visualize what output this code is trying to
        // achieve, take a look at the following list of consecutive tags:
        //
        // 0000001001100100
        // 0000001001101000
        // 0000001001110000
        // 0000001010000011
        let trailing_zero_filled_tag = tag | (tag - 1);
        let most_significant_one_of_least_significant_one_group_shifted_left_with_rest_of_tag =
            trailing_zero_filled_tag.wrapping_add(1);
        let shifted_back_rest_of_group = (tag
            ^ most_significant_one_of_least_significant_one_group_shifted_left_with_rest_of_tag)
            >> (tag.trailing_zeros() + 2);
        let next_tag =
            most_significant_one_of_least_significant_one_group_shifted_left_with_rest_of_tag
                ^ shifted_back_rest_of_group;

        Some(next_tag)
    })
    .take(BLOOM_FILTER_TAGS)
    .collect::<Vec<_>>();

    bloom_filter_tags.shuffle(rng);

    rustifact::write_const_array!(BLOOM_FILTER_TAGS, u16, &bloom_filter_tags);
}

fn send_cargo_instructions() {
    // Don't default to scanning the whole package directory for changes: this
    // script is independent.
    println!("cargo::rerun-if-changed=build.rs");
}

fn main() {
    let mut rng = get_rng();

    write_bloom_filter_tags(&mut rng);

    send_cargo_instructions();
}
